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
        P.option mempty
            $ snd <$> Lex.angles (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)

    (_, params) <-
        Lex.parens (P.sepBy (P.parseAnnotation' (snd <$> Typ.parseType)) Lex.comma)

    (_, ret) <- Lex.symbol "->" *> Typ.parseType

    ((_, end), body) <- P.parseExprBlock

    pure
        ( (start, end)
        ,
            [ HLIR.MkTopFunctionDeclaration
                { HLIR.name = HLIR.MkAnnotation idt generics
                , HLIR.parameters = params
                , HLIR.returnType = ret
                , HLIR.body = body
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
        P.option mempty
            $ snd <$> Lex.angles (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)

    ((_, end), aliased) <- Lex.symbol "=" *> Typ.parseType

    pure
        ( (start, end)
        ,
            [ HLIR.MkTopTypeAlias
                { HLIR.name = HLIR.MkAnnotation idt generics
                , HLIR.boundType = aliased
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
    idt <- P.parseAnnotation (snd <$> Typ.parseType)
    ((_, end), expr) <- Lex.symbol "=" *> P.parseExprFull
    pure ((start, end), [HLIR.MkTopLet idt expr])

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
    ((_, end), nodes) <-
        Lex.braces (P.many (snd <$> parseTopFull <* P.optional Lex.semi))
    pure ((start, end), [HLIR.MkTopModuleDeclaration idt (concat nodes)])

-- | PARSE EXTERNAL FUNCTION NODE
-- | An external function node is a top-level construct that defines an
-- | external function. External functions are defined using the `extern`
-- | keyword.
-- | External functions are similar to function declarations, but they do
-- | not have a body. They are defined as follows:
-- |
-- | - extern fn <name>[<generic>*](<param>*): <return type>
parseTopExternalFunction ::
    (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopExternalFunction = do
    ((start, _), _) <- Lex.reserved "extern"
    ((_, end), _) <- Lex.reserved "fn"
    (_, idt) <- Lex.identifier

    generics <-
        P.option mempty
            $ snd <$> Lex.angles (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)

    (_, params) <-
        Lex.parens (P.sepBy (P.parseAnnotation' (snd <$> Typ.parseType)) Lex.comma)

    (_, ret) <- Lex.symbol "->" *> Typ.parseType

    pure
        ( (start, end)
        ,
            [ HLIR.MkTopExternalFunction
                { HLIR.name = HLIR.MkAnnotation idt generics
                , HLIR.parameters = params
                , HLIR.returnType = ret
                }
            ]
        )

-- | PARSE PROPERTY NODE
-- | A property node is a top-level construct that defines a property.
-- | A property is a function that is used to get or set a value of a
-- | specific type. Properties are defined using the `property` keyword.
-- | Properties are similar to functions, but they have a different syntax
-- | and semantics.
-- | Properties are defined as follows:
-- | - property <name>[<generic>*](<param>*): <return type>
parseTopProperty ::
    (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopProperty = do
    ((start, _), _) <- Lex.reserved "property"
    (_, idt) <- Lex.identifier
    generics <-
        P.option mempty
            $ snd <$> Lex.angles (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)

    (_, params) <-
        Lex.parens (P.sepBy (P.parseAnnotation' (snd <$> Typ.parseType)) Lex.comma)

    ((_, end), ret) <- Lex.symbol "->" *> Typ.parseType

    pure
        ( (start, end)
        ,
            [ HLIR.MkTopProperty
                { HLIR.header = HLIR.MkAnnotation idt generics
                , HLIR.parameters = params
                , HLIR.returnType = ret
                }
            ]
        )

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
    generics <-
        P.option mempty
            $ snd <$> Lex.angles (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)

    ((_, end), variants) <-
        Lex.braces $ P.sepBy parseVariant Lex.comma

    pure
        ( (start, end)
        ,
            [ HLIR.MkTopEnumeration
                { HLIR.name = HLIR.MkAnnotation idt generics
                , HLIR.constructors = Map.fromList variants
                }
            ]
        )
  where
    parseVariant = do
        (_, name) <- Lex.identifier
        associated <-
            P.optional $ snd <$> Lex.parens (P.sepBy (snd <$> Typ.parseType) Lex.comma)

        pure (name, associated)

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
    ((start, _), _) <- Lex.reserved "impl"
    void $ Lex.reserved "fn"

    (_, forType) <- Lex.parens $ do
        (_, idt) <- Lex.identifier
        void Lex.colon
        ty <- snd <$> Typ.parseType
        pure (HLIR.MkAnnotation idt ty)

    (_, idt) <- Lex.identifier
    generics <-
        P.option mempty
            $ snd <$> Lex.angles (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)

    (_, params) <-
        Lex.parens (P.sepBy (P.parseAnnotation' (snd <$> Typ.parseType)) Lex.comma)

    void $ Lex.symbol "->"
    returnType <- snd <$> Typ.parseType

    ((_, end), body) <- P.parseExprBlock

    pure
        ( (start, end)
        ,
            [ HLIR.MkTopImplementation
                { HLIR.forType = forType
                , HLIR.header = HLIR.MkAnnotation idt generics
                , HLIR.parameters = params
                , HLIR.returnType = returnType
                , HLIR.body = body
                }
            ]
        )

-- | PARSE ANNOTATION NODE
-- | An annotation node is a top-level construct that defines an annotation.
-- | Annotations are defined using the `#` symbol followed by an identifier.
-- | Annotations are similar to attributes in Rust. They are defined as follows:
-- | - #<name>
-- | - #<name>(<arg>, ...)
parseTopAnnotation ::
    (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopAnnotation = do
    ((start, _), _) <- Lex.symbol "#"
    void $ Lex.symbol "["

    args <-
        P.option mempty
            $ P.sepBy (snd <$> P.parseExprFull) Lex.comma

    void $ Lex.symbol "]"

    ((_, end), nodes) <-
        P.choice
            [ Lex.braces (concat <$> P.many (snd <$> parseTopFull <* P.optional Lex.semi))
            , parseTopFull
            ]

    pure ((start, end), HLIR.MkTopAnnotation args <$> nodes)

-- | PARSE EXTERN LET
-- | An extern let node is a top-level construct that defines an external
-- | variable. External variables are defined using the `extern let` keyword.
-- | External variables are similar to global variables, but they are defined
-- | outside of the current module. They are defined as follows:
-- | - extern let <name>: <type>
parseTopExternLet ::
    (MonadIO m) => P.Parser m (HLIR.Position, [HLIR.HLIR "toplevel"])
parseTopExternLet = do
    ((start, _), _) <- Lex.reserved "extern"
    void $ Lex.reserved "let"
    (_, idt) <- Lex.identifier
    void Lex.colon
    ((_, end), ty) <- Typ.parseType

    pure ((start, end), [HLIR.MkTopExternLet (HLIR.MkAnnotation idt ty)])

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
            [ parseTopConstantDeclaration
            , parseTopLet
            , parseTopFunctionDeclaration
            , parseTopTypeAlias
            , parseTopImport
            , parseTopPublic
            , parseTopModuleDeclaration
            , P.try parseTopExternalFunction
            , parseTopExternLet
            , parseTopProperty
            , parseTopImplementation
            , parseTopAnnotation
            , parseTopEnumeration
            ]

-- | Parse a complete Reality source file.
-- | A Reality source file is a sequence of top-level constructs.
-- | The parser will return a list of top-level nodes.
parseProgram :: (MonadIO m) => P.Parser m [HLIR.HLIR "toplevel"]
parseProgram = concat <$> P.many (snd <$> parseTopFull <* P.optional Lex.semi) <* P.eof
