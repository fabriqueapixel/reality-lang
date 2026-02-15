{-# LANGUAGE LambdaCase #-}
module Language.Reality.Frontend.Parser.Internal.Type where

import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Frontend.Parser.Lexer qualified as Lex
import Language.Reality.Syntax.HLIR qualified as HLIR
import qualified Data.List as List

data TypeField
    = Field Text HLIR.Type
    | Rest HLIR.Type
    deriving (Show, Eq)

parseTypeField :: (MonadIO m) => P.Parser m TypeField
parseTypeField = P.choice [
        do
            ((_, _), fieldName) <- Lex.identifier
            void $ Lex.symbol ":"
            Field fieldName . snd <$> parseType,

        Rest . snd <$> (Lex.symbol ".." *> parseType)
    ]

-- | TYPE
-- | Parse a type.
parseType :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.Type)
parseType =
    P.choice
        [ -- Function type constructor
          -- Defined as the following:
          --
          -- "fn" "(" type ("," type)* ")" "->" type
          do
            ((start, _), _) <- Lex.reserved "fn"
            tys <- snd <$> Lex.parens (P.sepBy (snd <$> parseType) Lex.comma)
            ((_, end), ret) <- Lex.symbol "->" *> parseType

            pure ((start, end), tys HLIR.:->: ret)
        , -- Record type constructor
          -- Defined as the following:
          --
          -- "struct" "{" (identifier ":" type)* "}"
          do
            ((start, _), _) <- Lex.symbol "{"
            fields <- P.sepBy parseTypeField Lex.comma

            ((_, end), _) <- Lex.symbol "}"

            let (rest, fields') = List.partition (\case Rest _ -> True; _ -> False) fields

            when (length rest > 1) $ fail "Multiple rest fields in record type"

            restType <- case rest of
                [] -> pure HLIR.MkTyRowEmpty
                [Rest ty] -> pure ty
                _ -> fail "Unreachable due to previous check"

            fields'' <- forM fields' $ \case
                Field name ty -> pure (name, ty)
                Rest _ -> fail "Unreachable due to previous check"

            let recordType = List.foldl (\acc (fieldName, fieldType) -> HLIR.MkTyRowExtend fieldName fieldType acc) restType fields''

            pure ((start, end), HLIR.MkTyRecord recordType)
        , -- Pointer type constructor
          -- Defined as the following:
          --
          -- "*" type
          do
            ((start, _), _) <- Lex.symbol "*"
            ((_, end), ty) <- parseType

            pure ((start, end), HLIR.MkTyPointer ty)
        , -- Tuple type constructor
          -- Defined as the following:
          --
          -- "(" type "," type ")"
          do
            (pos, ty) <- Lex.parens $ do
                x <- snd <$> parseType
                void $ Lex.reserved ","
                HLIR.MkTyTuple x . snd <$> parseType
            pure (pos, ty)
        , -- Type application constructor
          -- Defined as the following:
          --
          -- identifier "<" type ("," type)* ">"
          P.try $ do
            ((start, _), idt) <- Lex.identifier
            ((_, end), tys) <- Lex.angles $ P.sepBy1 (snd <$> parseType) Lex.comma

            pure ((start, end), HLIR.MkTyApp (HLIR.MkTyId idt) tys)
        , Lex.identifier <&> second HLIR.MkTyId
        ]
