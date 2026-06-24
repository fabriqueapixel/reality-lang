{-# LANGUAGE LambdaCase #-}
module Language.Reality.Frontend.Parser.Internal.Type where

import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Frontend.Parser.Lexer qualified as Lex
import Language.Reality.Syntax.HLIR qualified as HLIR
import qualified Data.List as List
import qualified Control.Monad.Combinators.Expr as P

fetchLastPosition :: [(HLIR.Position, a)] -> P.Parser m P.SourcePos
fetchLastPosition [] = P.getSourcePos
fetchLastPosition [((_, pos), _)] = pure pos
fetchLastPosition (_ : xs) = fetchLastPosition xs

data TypeField
    = Field Text HLIR.Type Bool
    | Rest HLIR.Type
    deriving (Show, Eq)

parseTypeField :: (MonadIO m) => P.Parser m (HLIR.Position, TypeField)
parseTypeField = P.choice [
        do
            ((start, _), fieldName) <- Lex.identifier
            isOpt <- P.option False (Lex.symbol "?" $> True)
            void $ Lex.symbol ":"
            ((_, end), fieldType) <- parseType

            pure ((start, end), Field fieldName fieldType isOpt),

        do
            ((start, _), _) <- Lex.symbol ".."
            ((_, end), fieldType) <- parseType

            pure ((start, end), Rest fieldType)
    ]


parseType :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.Type)
parseType = P.makeExprParser parseTypeTerm operatorTable
  where
    operatorTable =
        [ [
            P.InfixL (Lex.symbol "->" $>
              (\(pos1, ty1) (pos2, ty2) -> ((fst pos1, snd pos2), [ty1] HLIR.:->: ty2))
            )
          ]
        ]

parseTypeRecord :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.Type)
parseTypeRecord = do
  ((start, _), _) <- Lex.reserved "{"
  fields <- Lex.indentedSepBy (snd <$> parseTypeField) Lex.comma
  void Lex.scn
  void $ Lex.reserved "}"

  end <- fetchLastPosition fields
  let fields' = map snd fields

  let (rest, fields'') = List.partition (\case Rest _ -> True; _ -> False) fields'

  when (length rest > 1) $ fail "Multiple rest fields in record type"

  restType <- case rest of
      [] -> pure HLIR.MkTyRowEmpty
      [Rest ty] -> pure ty
      _ -> fail "Unreachable due to previous check"

  fields''' <- forM fields'' $ \case
      Field name ty isOpt -> pure (name, ty, isOpt)
      Rest _ -> fail "Unreachable due to previous check"

  let recordType = List.foldl (\acc (fieldName, fieldType, isOpt) -> HLIR.MkTyRowExtend fieldName fieldType isOpt acc) restType fields'''

  pure ((start, end), HLIR.MkTyRecord recordType)

parseTypePointer :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.Type)
parseTypePointer = do
  ((start, _), _) <- Lex.symbol "*"
  ((_, end), ty) <- parseType

  pure ((start, end), HLIR.MkTyPointer ty)

parseTypeTuple :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.Type)
parseTypeTuple = do
  (pos, ty) <- Lex.parens $ do
      x <- snd <$> parseType
      void $ Lex.reserved ","
      HLIR.MkTyTuple x . snd <$> parseType
  pure (pos, ty)

-- | TYPE
-- | Parse a type.
parseTypeTerm :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.Type)
parseTypeTerm =
    P.choice
        [ -- Record type constructor
          -- Defined as the following:
          --
          -- "{" (identifier ":" type)* "}"
          parseTypeRecord

        , -- Pointer type constructor
          -- Defined as the following:
          --
          -- "*" type
          parseTypePointer

        , -- Tuple type constructor
          -- Defined as the following:
          --
          -- "(" type "," type ")"
          parseTypeTuple

        , -- Type application constructor
          -- Defined as the following:
          --
          -- identifier "<" type ("," type)* ">"
          P.try $ do
            ((start, _), idt) <- Lex.identifier
            tys <- P.some $ P.choice [
                Lex.symbol "'" *> Lex.identifier <&> second HLIR.MkTyQuantified
              , Lex.identifier <&> second HLIR.MkTyId
              , parseTypeRecord
              , parseTypePointer
              , parseTypeTuple
              ]

            end <- fetchLastPosition tys
            let tys' = map snd tys

            pure ((start, end), buildTypeApp (HLIR.MkTyId idt) tys')

        , -- Type variable
          -- Defined as the following:
          --
          -- 'identifier
          Lex.symbol "'" *> Lex.identifier <&> second HLIR.MkTyQuantified

        , Lex.identifier <&> second HLIR.MkTyId
        ]

buildTypeApp :: HLIR.Type -> [HLIR.Type] -> HLIR.Type
buildTypeApp = List.foldl (\ t arg -> HLIR.MkTyApp t [arg])
