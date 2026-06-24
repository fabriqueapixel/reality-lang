module Language.Reality.Frontend.Parser.Toplevel where

import Data.List qualified as List
import Data.Map qualified as Map
import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Frontend.Parser.Expression qualified as P
import Language.Reality.Frontend.Parser.Internal.Type qualified as Typ
import Language.Reality.Frontend.Parser.Lexer qualified as Lex
import Language.Reality.Syntax.HLIR qualified as HLIR
import Text.Megaparsec.Char qualified as P

-- | PARSE CONSTANT DECLARATION NODE
-- | A constant declaration node is a top-level construct that defines a
-- | constant value. Constants are defined using the `const` keyword.
-- | Constants are similar to variables, but they cannot be reassigned.
-- | Constants are defined as follows:
-- |
-- | - const <name>: <type> = <expression>
parseTopConstantDeclaration ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopConstantDeclaration = do
  ((start, _), _) <- Lex.reserved "const"
  idt <- P.parseAnnotation' (snd <$> Typ.parseType)
  ((_, end), expr) <- Lex.symbol "=" *> P.parseExprFull

  pure ((start, end), [HLIR.MkTopConstantDeclaration idt expr])

someT :: HLIR.Type -> HLIR.Type
someT ty =
  HLIR.MkTyApp (HLIR.MkTyId "Option") [ty]

parseArg :: (MonadIO m) => P.Parser m (HLIR.Annotation HLIR.Type)
parseArg = (snd <$>) . Lex.parens $ P.parseAnnotation' (snd <$> Typ.parseType)

parseTopExternalFunctionDeclaration ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopExternalFunctionDeclaration = do
  ((start, _), _) <- Lex.reserved "extern"
  void $ Lex.reserved "fn"

  (_, idt) <- Lex.identifier
  generics <-
    P.option mempty $ some (P.char '\'' *> (snd <$> Lex.identifier))
  preParams <- P.many parseArg
  ((_, preEnd), ret) <- Lex.symbol ":" *> Typ.parseType

  pure
    ( (start, preEnd),
      [ HLIR.MkTopExternalFunction
          { HLIR.name = HLIR.MkAnnotation idt generics,
            HLIR.extTypeValue = List.foldr (\ann acc -> [ann.typeValue] `HLIR.MkTyFun` acc) ret preParams
          }
      ]
    )

-- | PARSE FUNCTION DECLARATION NODE
-- | A function declaration node is a top-level construct that defines a
-- | function. Functions are defined using the `fn` keyword.
-- | Functions are defined as follows:
-- |
-- | - fn <name>[<generic>*](<param>*): <return type> { <body> }
parseTopFunctionDeclaration ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopFunctionDeclaration = do
  ((start, _), _) <- Lex.reserved "fn"
  (_, idt) <- Lex.identifier

  generics <-
    P.option mempty $ some (P.char '\'' *> (snd <$> Lex.identifier))

  preParams <- some parseArg

  ((_, preEnd), ret) <- Lex.symbol ":" *> Typ.parseType

  body <- P.optional $ do
    void $ Lex.symbol "="

    ((_, end), body) <- P.parseExprFull

    pure (end, body)

  case preParams of
    [] -> fail "Function must have at least one parameter"
    params@(firstParam : restParams) -> do

      case body of
        Nothing -> do
          let newRet = List.foldr (\ann acc -> [ann.typeValue] `HLIR.MkTyFun` acc) ret params

          pure
            ( (start, preEnd),
              [ HLIR.MkTopProperty
                  { HLIR.header = HLIR.MkAnnotation idt generics,
                    HLIR.propTypeValue = newRet
                  }
              ]
            )
        Just (end, bodyExpr) -> do
          let newBody =
                List.foldr
                  ( \ann acc ->
                      HLIR.MkExprLambda
                        { HLIR.parameters = [Just <$> ann],
                          HLIR.returnType = Nothing,
                          HLIR.body = acc
                        }
                  )
                  bodyExpr
                  restParams

          let newRet = List.foldr (\ann acc -> [ann.typeValue] `HLIR.MkTyFun` acc) ret restParams

          pure
            ( (start, end),
              [ HLIR.MkTopFunctionDeclaration
                  { HLIR.name = HLIR.MkAnnotation idt generics,
                    HLIR.parameters = [firstParam],
                    HLIR.returnType = newRet,
                    HLIR.body = newBody
                  }
              ]
            )

parseTopPatternAlias ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopPatternAlias = do
  ((start, _), _) <- Lex.reserved "pattern"
  (_, idt) <- Lex.identifier  

  generics <-
    P.option mempty $ some (P.char '\'' *> (snd <$> Lex.identifier))

  args <- P.many parseArg

  (_, retType) <- Lex.symbol ":" *> Typ.parseType

  void $ Lex.symbol "="

  ((_, end), pat) <- P.parsePatternFull

  pure
    ( (start, end),
      [ HLIR.MkTopPatternAlias
          { HLIR.bindingWithGenerics = HLIR.MkAnnotation idt generics,
            HLIR.arguments = args,
            HLIR.patternBinding = pat,
            HLIR.returnType = retType
          }
      ]
    )

-- | PARSE TYPE ALIAS NODE
-- | A type alias node is a top-level construct that defines a type
-- | alias. Type aliases are defined using the `type` keyword.
-- | Type aliases are similar to typedefs in C/C++ or type aliases in
-- | TypeScript. They are defined as follows:
-- |
-- | - type <name>[<generic>*] = <type>
parseTopTypeAlias ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopTypeAlias = do
  ((start, _), _) <- Lex.reserved "type"

  (_, idt) <- Lex.identifier

  generics <-
    P.option mempty $ some (P.char '\'' *> (snd <$> Lex.identifier))

  ((_, end), aliased) <- Lex.symbol "=" *> Typ.parseType

  pure
    ( (start, end),
      [ HLIR.MkTopTypeAlias
          { HLIR.name = HLIR.MkAnnotation idt generics,
            HLIR.boundType = aliased
          }
      ]
    )

-- | PARSE LET NODE
-- | A let node is a top-level construct that defines a variable. Variables
-- | are defined using the `let` keyword. Variables are similar to constants,
-- | but they can be reassigned. Variables are defined as follows:
-- | - let <name>: <type> = <expression>
parseTopLet ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopLet = do
  ((start, _), _) <- Lex.reserved "let"
  idt <- P.parseAnnotation' (snd <$> Typ.parseType)

  end <- P.getSourcePos

  optExpr <- P.optional $ do
    void $ Lex.symbol "="
    P.parseExprFull

  pure $ case optExpr of
    Nothing -> ((start, end), [HLIR.MkTopExternLet idt])
    Just ((_, end'), expr) -> ((start, end'), [HLIR.MkTopLet (Just <$> idt) expr])

-- | PARSE IMPORT NODE
-- | An import node is a top-level construct that defines an import
-- | statement. Import statements are defined using the `import` keyword.
-- | Import statements are similar to import statements in Rust.
-- | They are defined as follows:
-- |
-- | - import <module>::<submodule>::...::<name>
-- | - import *::<submodule>::...::<name>
parseTopImport ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopImport = do
  ((start, _), _) <- Lex.reserved "import"
  modules <-
    P.sepBy1 Lex.nonLexedID (P.string ".") P.<?> "module path"

  let lastPosition = if null modules then start else List.maximum (map (snd . fst) modules)
  let moduleParts = map snd modules

  pure ((start, lastPosition), [HLIR.MkTopImport moduleParts])

-- | PARSE PUBLIC NODE
-- | A public node is a top-level construct that defines a public
-- | declaration. Public declarations are defined using the `pub` keyword.
-- | Public declarations are similar to public declarations in Rust.
-- | They are defined as follows:
-- |
-- | - pub <declaration>
parseTopPublic ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopPublic = do
  ((start, _), _) <- Lex.reserved "pub"
  (pos, node) <- parseTopFull
  pure ((start, snd pos), HLIR.MkTopPublic <$> node)

-- | PARSE MODULE DECLARATION NODE
-- | A module declaration node is a top-level construct that defines a
-- | module. Modules are defined using the `mod` keyword.
-- | Modules are similar to modules in Rust. They are defined as follows:
-- |
-- | - mod <name> { <declaration>* }
parseTopModuleDeclaration ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopModuleDeclaration = do
  ((start, _), _) <- Lex.reserved "mod"
  (_, idt) <- Lex.identifier

  void $ Lex.symbol "="

  nodes <- Lex.indented parseTopFull

  end <- fetchLastPosition nodes
  let nodes' = concatMap snd nodes

  pure ((start, end), [HLIR.MkTopModuleDeclaration idt nodes'])

-- | PARSE TOPLEVEL ENUMERATION
-- | An enumeration node is a top-level construct that defines an
-- | enumeration. Enumerations are defined using the `enum` keyword.
-- | Enumerations are similar to enums in Rust. They are defined as follows:
-- |
-- | - enum <name>[<generic>*] { <variant>, ... }
-- |
-- | Variants can be simple identifiers or can have associated data:
-- |
-- | - <variant>
-- | - <variant>(<type>, ...)
parseTopEnumeration ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopEnumeration = do
  ((start, _), _) <- Lex.reserved "enum"
  (_, idt) <- Lex.identifier
  generics <- P.option mempty $ some (P.char '\'' *> (snd <$> Lex.identifier))

  void $ Lex.symbol "="

  variants <- Lex.indented parseVariant

  end <- P.getSourcePos

  pure
    ( (start, end),
      [ HLIR.MkTopEnumeration
          { HLIR.name = HLIR.MkAnnotation idt generics,
            HLIR.constructors = Map.fromList variants
          }
      ]
    )
  where
    parseVariant = do
      (_, name) <- Lex.identifier
      associated <-
        P.optional $ P.some parseArg

      pure (name, fmap (map (.typeValue)) associated)

fetchLastPosition :: [(HLIR.Position, a)] -> P.Parser m P.SourcePos
fetchLastPosition [] = P.getSourcePos
fetchLastPosition [((_, pos), _)] = pure pos
fetchLastPosition (_ : xs) = fetchLastPosition xs

-- | PARSE IMPLEMENTATION NODE
-- | An implementation node is a top-level construct that defines an
-- | implementation of a property for a specific type.
-- | Implementations are defined using the `impl` keyword.
-- | Implementations are similar to functions, but they have a different
-- | syntax and semantics.
-- | Implementations are defined as follows:
-- | - impl fn (<for>: <type>) <name>[<generic>*](<param>*): <return type> { <body> }
parseTopImplementation ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopImplementation = do
  ((start, _), _) <- Lex.reserved "overload"

  (_, forType) <- Lex.parens $ do
    (_, idt) <- Lex.identifier
    void Lex.colon
    ty <- snd <$> Typ.parseType
    pure (HLIR.MkAnnotation idt ty)

  void $ Lex.symbol "="

  nodes <- Lex.indented $ do
    ((start', _), _) <- Lex.reserved "fn"
    (_, idt) <- Lex.identifier

    generics <- P.option mempty $
      P.some (P.char '\'' *> (snd <$> Lex.identifier))

    preParams <- P.many parseArg

    ((_, preEnd), returnType) <- Lex.symbol ":" *> Typ.parseType

    body <- P.optional $ do
      void $ Lex.symbol "="
      ((_, end), body) <- P.parseExprFull

      pure (end, body)

    pure (start', idt, generics, preParams, preEnd, returnType, body)

  let fetchLastPosition' ::
        MonadIO m =>
        [(a, b, c, d, P.SourcePos, e, Maybe (P.SourcePos, f))] ->
        P.Parser m P.SourcePos
      fetchLastPosition' [] = P.getSourcePos
      fetchLastPosition' [(_, _, _, _, pos, _, mPos)] = case mPos of
        Nothing -> pure pos
        Just (endPos, _) -> pure endPos
      fetchLastPosition' (_ : xs) = fetchLastPosition' xs

  end <- fetchLastPosition' nodes

  pure ((start, end), flip map nodes $ \(start', idt, generics, preParams, preEnd, returnType, body) ->
    case body of
      Nothing -> do
        let newRet = List.foldr (\ann acc -> [ann.typeValue] `HLIR.MkTyFun` acc) returnType (forType : preParams)

        flip HLIR.locate (start', preEnd) $ HLIR.MkTopProperty {
          header = HLIR.MkAnnotation idt generics,
          propTypeValue = newRet
        }

      Just (end', bodyExpr) -> do

        let newBody =
              List.foldr
                ( \ann acc ->
                    HLIR.MkExprLambda
                      { HLIR.parameters = [Just <$> ann],
                        HLIR.returnType = Nothing,
                        HLIR.body = acc
                      }
                )
                bodyExpr
                preParams

        let newRet = List.foldr (\ann acc -> [ann.typeValue] `HLIR.MkTyFun` acc) returnType preParams

        flip HLIR.locate (start', end') $ HLIR.MkTopFunctionDeclaration
          { HLIR.name = HLIR.MkAnnotation idt generics,
            HLIR.parameters = [forType],
            HLIR.returnType = newRet,
            HLIR.body = newBody
          })

-- | PARSE ANNOTATION NODE
-- | An annotation node is a top-level construct that defines an annotation.
-- | Annotations are defined using the `#` symbol followed by an identifier.
-- | Annotations are similar to attributes in Rust. They are defined as follows:
-- | - #<name>
-- | - #<name>(<arg>, ...)
parseTopAnnotation ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopAnnotation = do
  ((start, _), _) <- Lex.reserved "pragma"

  args <- P.some . P.try $ P.parseExprTerm False

  void $ Lex.symbol "="

  nodes <- Lex.indented parseTopFull

  let nodes' = concatMap snd nodes
  end <- fetchLastPosition nodes

  pure ((start, end), HLIR.MkTopAnnotation (map snd args) <$> nodes')

-- | TOP LEVEL PARSING
-- | A top-level parser is a parser that parses top-level constructs in a
-- | programming language. Top-level constructs are constructs that are not
-- | nested inside other constructs. For instance, in Reality, top-level
-- | constructs are:
-- | - Constant declarations
-- | - Function declarations
-- | - Type aliases
-- | - Import statements
-- | - Public declarations
-- | - Module declarations
-- | - Structure declarations
-- | - External function declarations
-- |
-- | The top-level parser is responsible for parsing these constructs and
-- | returning them as a list of top-level nodes.
parseTopFull ::
  (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopFull =
  Lex.locateWith
    <$> P.choice
      [ parseTopConstantDeclaration,
        parseTopLet,
        parseTopPatternAlias,
        parseTopExternalFunctionDeclaration,
        parseTopFunctionDeclaration,
        parseTopTypeAlias,
        parseTopImport,
        parseTopPublic,
        parseTopModuleDeclaration,
        parseTopImplementation,
        parseTopAnnotation,
        parseTopEnumeration
      ]

-- | Parse a complete Reality source file.
-- | A Reality source file is a sequence of top-level constructs.
-- | The parser will return a list of top-level nodes.
parseProgram :: (MonadIO m) => P.Parser m [HLIR.HLIR "toplevel"]
parseProgram = concat . concat <$> P.many (Lex.nonIndented $ snd <$> parseTopFull) <* Lex.scn <* P.eof
