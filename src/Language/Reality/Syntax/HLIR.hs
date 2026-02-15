{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Reality.Syntax.HLIR (
    Expression (..),
    Toplevel (..),
    Pattern (..),
    StructureMember (..),
    -- Patterns
    pattern MkExprBinary,
    pattern MkExprString,
    pattern MkExprTuple,
    pattern MkExprVarCall,
    pattern MkExprWhileIs,
    pattern MkExprConditionIs,
    -- Re-exports
    module Lit,
    module Ann,
    module Pos,
    module Ty,
    -- Type families
    HLIR,
    TLIR,
    -- Functions
    getFirstAnnotationArgument,
)
where

import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.TypeLits (Symbol)
import Language.Reality.Syntax.Internal.Annotation as Ann
import Language.Reality.Syntax.Internal.Literal as Lit
import Language.Reality.Syntax.Internal.Position as Pos
import Language.Reality.Syntax.Internal.Type as Ty
import Prelude hiding (Type)

data Expression f t
    = MkExprApplication
        { callee :: Expression f t
        , arguments :: [Expression f t]
        , returnType :: f t
        }
    | MkExprVariable (Ann.Annotation (f t)) [t]
    | MkExprLiteral Lit.Literal
    | MkExprLambda
        { parameters :: [Ann.Annotation (f t)]
        , returnType :: f t
        , body :: Expression f t
        }
    | MkExprLetIn
        { binding :: Ann.Annotation (f t)
        , value :: Expression f t
        , inExpr :: Expression f t
        , returnType :: f t
        }
    | MkExprCondition
        { condition :: Expression f t
        , thenBranch :: Expression f t
        , elseBranch :: Expression f t
        , thenType :: f t
        , elseType :: f t
        }
    | MkExprSingleIf (Expression f t) (Expression f t) (f t)
    | MkExprLocated
        { span :: Position
        , expr :: Expression f t
        }
    | MkExprStructureAccess
        { structure :: Expression f t
        , field :: Text
        }
    | MkExprStructureCreation
        { field :: Text
        , value :: Expression f t
        , record :: Expression f t
        , fieldType :: f t
        , recordType :: f t
        }
    | MkExprStructureEmpty
    | MkExprDereference (Expression f t) (f t)
    | MkExprReference (Expression f t) (f t)
    | MkExprUpdate
        { update :: Expression f t
        , value :: Expression f t
        , updateType :: f t
        }
    | MkExprSizeOf t
    | MkExprCast (Expression f t) t
    | MkExprWhile
        { condition :: Expression f t
        , body :: Expression f t
        , returnType :: f t
        , inExpr :: Expression f t
        }
    | MkExprFunctionAccess
        { field :: Text
        , fieldExpr :: Expression f t
        , typeValues :: [t]
        , arguments :: [Expression f t]
        }
    | MkExprReturn (Expression f t)
    | MkExprBreak
    | MkExprContinue
    | MkExprIs (Expression f t) (Pattern f t) (f t)
    | MkExprLetPatternIn
        { patternBinding :: Pattern f t
        , value :: Expression f t
        , inExpr :: Expression f t
        , returnType :: f t
        }
    deriving (Eq, Ord, Generic)

data Toplevel f t
    = MkTopConstantDeclaration
        { binding :: Ann.Annotation t
        , value :: Expression f t
        }
    | MkTopFunctionDeclaration
        { name :: Ann.Annotation [Text]
        , parameters :: [Ann.Annotation t]
        , returnType :: t
        , body :: Expression f t
        }
    | MkTopImport [Text]
    | MkTopTypeAlias
        { name :: Ann.Annotation [Text]
        , boundType :: t
        }
    | MkTopPublic (Toplevel f t)
    | MkTopModuleDeclaration
        { moduleName :: Text
        , nodes :: [Toplevel f t]
        }
    | MkTopLocated
        { span :: Position
        , node :: Toplevel f t
        }
    | MkTopExternalFunction
        { name :: Ann.Annotation [Text]
        , parameters :: [Ann.Annotation t]
        , returnType :: t
        }
    | MkTopProperty
        { header :: Ann.Annotation [Text]
        , parameters :: [Ann.Annotation t]
        , returnType :: t
        }
    | MkTopImplementation
        { forType :: Ann.Annotation t
        , header :: Ann.Annotation [Text]
        , parameters :: [Ann.Annotation t]
        , returnType :: t
        , body :: Expression f t
        }
    | MkTopLet
        { optBinding :: Ann.Annotation (f t)
        , value :: Expression f t
        }
    | MkTopAnnotation [Expression f t] (Toplevel f t)
    | MkTopExternLet (Ann.Annotation t)
    | MkTopEnumeration
        { name :: Ann.Annotation [Text]
        , constructors :: Map Text (Maybe [t])
        }
    deriving (Eq, Ord, Generic)

-- | PATTERN
data Pattern f t
    = MkPatternVariable (Ann.Annotation (f t))
    | MkPatternLet (Ann.Annotation (f t))
    | MkPatternLiteral Lit.Literal
    | MkPatternWildcard
    | MkPatternStructure (Map Text (Pattern f t))
    | MkPatternConstructor Text [Pattern f t] (f t) -- Constructor name and its associated patterns
    | MkPatternLocated
        { span :: Position
        , patternNode :: Pattern f t
        }
    deriving (Eq, Ord, Generic)

data StructureMember t
    = MkStructField Text t
    | MkStructStruct Text [StructureMember t]
    | MkStructUnion Text [StructureMember t]
    deriving (Eq, Ord, Show, Generic)

-- | BINARY EXPRESSION PATTERN
-- | A pattern synonym to represent binary expressions in Reality.
pattern MkExprBinary ::
    Text -> Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprBinary op a b =
    MkExprApplication (MkExprVariable (MkAnnotation op Nothing) []) [a, b] Nothing

pattern MkExprWhileIs ::
    Expression Maybe t ->
    Pattern Maybe t ->
    Expression Maybe t ->
    Expression Maybe t ->
    Expression Maybe t
pattern MkExprWhileIs cond pat body inExpr =
    MkExprWhile
        { condition = MkExprLiteral (Lit.MkLitBool True)
        , body = MkExprCondition
            { condition = MkExprIs cond pat Nothing
            , thenBranch = body
            , elseBranch = MkExprBreak
            , thenType = Nothing
            , elseType = Nothing
            }
        , returnType = Nothing
        , inExpr = inExpr
        }

pattern MkExprVarCall ::
    Text -> [Expression Maybe t] -> Expression Maybe t
pattern MkExprVarCall name args =
    MkExprApplication (MkExprVariable (MkAnnotation name Nothing) []) args Nothing

-- |  STRING EXPRESSION PATTERN
-- | A pattern synonym to represent string expressions in Reality.
pattern MkExprString :: Text -> Expression f t
pattern MkExprString s = MkExprLiteral (MkLitString s)

-- | TUPLE EXPRESSION PATTERN
-- | A pattern synonym to represent tuple expressions in Reality.
pattern MkExprTuple ::
    Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprTuple a b =
    MkExprApplication
        (MkExprVariable (MkAnnotation "Pair" Nothing) [])
        [a, b]
        Nothing

pattern MkExprConditionIs ::
    Expression Maybe t ->
    Pattern Maybe t ->
    Expression Maybe t ->
    Expression Maybe t ->
    Expression Maybe t
pattern MkExprConditionIs cond pat thenB elseB =
    MkExprCondition
        { condition = MkExprIs cond pat Nothing
        , thenBranch = thenB
        , elseBranch = elseB
        , thenType = Nothing
        , elseType = Nothing
        }

type family HLIR (s :: Symbol) where
    HLIR "expression" = Expression Maybe Type
    HLIR "toplevel" = Toplevel Maybe Type
    HLIR "pattern" = Pattern Maybe Type

type family TLIR (s :: Symbol) where
    TLIR "expression" = Expression Identity Type
    TLIR "toplevel" = Toplevel Identity Type
    TLIR "pattern" = Pattern Identity Type

getFirstAnnotationArgument :: Expression f t -> Maybe Text
getFirstAnnotationArgument (MkExprVariable ann _) = pure ann.name
getFirstAnnotationArgument (MkExprLocated _ e) = getFirstAnnotationArgument e
getFirstAnnotationArgument _ = Nothing

instance Locate (Expression f t) where
    locate e p = MkExprLocated{span = p, expr = e}

instance Locate (Toplevel f t) where
    locate e p = MkTopLocated{span = p, node = e}

instance Locate (Pattern f t) where
    locate p pos = MkPatternLocated{span = pos, patternNode = p}

instance (ToText (f t), ToText t) => ToText (Expression f t) where
    toText (MkExprApplication callee args _) =
        T.concat [toText callee, "(", T.intercalate ", " (map toText args), ")"]
    toText (MkExprVariable ann _) = toText ann.name
    toText (MkExprLiteral lit) = toText lit
    toText (MkExprLambda params ret body) =
        T.concat
            [ "\\("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText ret
            , " { "
            , toText body
            , " }"
            ]
    toText (MkExprLetIn binding value inExpr _) =
        T.concat
            [ "let "
            , toText binding
            , " = "
            , toText value
            , " in "
            , toText inExpr
            ]
    toText (MkExprCondition cond thenB elseB _ _) =
        T.concat
            [ "if "
            , toText cond
            , " then "
            , toText thenB
            , " else "
            , toText elseB
            ]
    toText (MkExprLocated _ e) = toText e
    toText (MkExprStructureAccess struct field) =
        T.concat [toText struct, ".", field]
    toText MkExprStructureEmpty = "{}"
    toText (MkExprStructureCreation field value record _ _) =
        T.concat [toText record, " { ", field, " = ", toText value, " }"]
    toText (MkExprDereference e _) = T.concat ["*", toText e]
    toText (MkExprReference e _) = T.concat ["&", toText e]
    toText (MkExprUpdate update value _) =
        T.concat [toText update, " = ", toText value]
    toText (MkExprSizeOf t) = T.concat ["sizeof(", toText t, ")"]
    toText (MkExprSingleIf cond thenB _) =
        T.concat ["if ", toText cond, " then ", toText thenB]
    toText (MkExprCast e t) = T.concat ["(", toText e, " as ", toText t, ")"]
    toText (MkExprWhile cond body _ inExpr) =
        T.concat
            [ "while "
            , toText cond
            , " { "
            , toText body
            , " } in "
            , toText inExpr
            ]
    toText (MkExprFunctionAccess func fieldExpr _ args) =
        T.concat
            [ toText func
            , "("
            , toText fieldExpr
            , ")("
            , T.intercalate ", " (map toText args)
            , ")"
            ]
    toText (MkExprReturn expr) = "return " <> toText expr
    toText MkExprBreak = "break"
    toText MkExprContinue = "continue"
    toText (MkExprIs expr pat _) =
        T.concat ["is(", toText expr, ", ", toText pat, ")"]
    toText (MkExprLetPatternIn pat value inExpr _) =
        T.concat
            [ "let "
            , toText pat
            , " = "
            , toText value
            , " in "
            , toText inExpr
            ]

instance (ToText (f t), ToText t) => ToText (Pattern f t) where
    toText (MkPatternVariable ann) = toText ann
    toText (MkPatternLet ann) = "let " <> toText ann
    toText (MkPatternLiteral lit) = toText lit
    toText MkPatternWildcard = "_"
    toText (MkPatternStructure fields) =
        let fieldTexts = map (\(name, pat) -> name <> ": " <> toText pat) (Map.toList fields)
         in T.concat ["{ ", T.intercalate ", " fieldTexts, " }"]
    toText (MkPatternConstructor name pats _) =
        T.concat [name, "(", T.intercalate ", " (map toText pats), ")"]
    toText (MkPatternLocated _ p) = toText p

instance (ToText (f t), ToText t) => ToText (Toplevel f t) where
    toText (MkTopConstantDeclaration binding value) =
        T.concat ["const ", toText binding, " = ", toText value]
    toText (MkTopFunctionDeclaration name params ret body) =
        T.concat
            [ "fn "
            , name.name
            , "["
            , T.intercalate ", " name.typeValue
            , "]"
            , "("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText ret
            , " { "
            , toText body
            , " }"
            ]
    toText (MkTopImport mods) =
        T.concat ["import ", T.intercalate "." mods]
    toText (MkTopTypeAlias name typeValue) =
        T.concat
            [ "type "
            , name.name
            , "["
            , T.intercalate ", " name.typeValue
            , "]"
            , " = "
            , toText typeValue
            ]
    toText (MkTopPublic node) = T.concat ["public ", toText node]
    toText (MkTopModuleDeclaration modName nodes) =
        T.concat
            [ "mod "
            , modName
            , " { "
            , T.intercalate "\n" (map toText nodes)
            , " }"
            ]
    toText (MkTopLocated _ n) = toText n
    toText (MkTopExternalFunction name params ret) =
        T.concat
            [ "extern fn "
            , name.name
            , "["
            , T.intercalate ", " name.typeValue
            , "]"
            , "("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText ret
            ]
    toText (MkTopProperty header params ret) =
        T.concat
            [ "property "
            , header.name
            , "["
            , T.intercalate ", " header.typeValue
            , "]"
            , "("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText ret
            ]
    toText (MkTopImplementation forType header params returnType body) =
        T.concat
            [ "impl fn "
            , header.name
            , "["
            , T.intercalate ", " header.typeValue
            , "] for "
            , toText forType
            , "("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText returnType
            , " { "
            , toText body
            , " }"
            ]
    toText (MkTopAnnotation exprs node) =
        T.concat
            [ "#["
            , T.intercalate ", " (map toText exprs)
            , "]\n"
            , toText node
            ]
    toText (MkTopExternLet binding) =
        T.concat ["extern let ", toText binding]
    toText (MkTopEnumeration name constructors) =
        let constructorTexts = map formatConstructor (Map.toList constructors)
            formatConstructor (cName, Nothing) = cName
            formatConstructor (cName, Just ty) = cName <> "(" <> T.intercalate ", " (map toText ty) <> ")"
         in T.concat
                [ "enum "
                , name.name
                , "["
                , T.intercalate ", " name.typeValue
                , "]"
                , " { "
                , T.intercalate ", " constructorTexts
                , " }"
                ]
    toText (MkTopLet binding value) =
        T.concat ["let ", toText binding, " = ", toText value]

instance ToText t => ToText (StructureMember t) where
    toText (MkStructField name ty) =
        T.concat ["  ", name, ": ", toText ty, ";"]
    toText (MkStructStruct name fields) =
        T.concat
            [ "  struct "
            , name
            , " {\n"
            , T.intercalate
                "\n"
                (map (\f -> "    " <> toText f) fields)
            , "\n  };"
            ]
    toText (MkStructUnion name fields) =
        T.concat
            [ "  union "
            , name
            , " {\n"
            , T.intercalate
                "\n"
                (map (\f -> "    " <> toText f) fields)
            , "\n  };"
            ]

instance Functor StructureMember where
    fmap f (MkStructField name ty) = MkStructField name (f ty)
    fmap f (MkStructStruct name fields) = MkStructStruct name (map (fmap f) fields)
    fmap f (MkStructUnion name fields) = MkStructUnion name (map (fmap f) fields)

instance Foldable StructureMember where
    foldMap f (MkStructField _ ty) = f ty
    foldMap f (MkStructStruct _ fields) = foldMap (foldMap f) fields
    foldMap f (MkStructUnion _ fields) = foldMap (foldMap f) fields

instance Traversable StructureMember where
    traverse f (MkStructField name ty) = MkStructField name <$> f ty
    traverse f (MkStructStruct name fields) = MkStructStruct name <$> traverse (traverse f) fields
    traverse f (MkStructUnion name fields) = MkStructUnion name <$> traverse (traverse f) fields

instance Applicative StructureMember where
    pure = MkStructField ""
    (MkStructField _ fTy) <*> sm = fmap fTy sm
    (MkStructStruct name fFields) <*> sm = MkStructStruct name (map (<*> sm) fFields)
    (MkStructUnion name fFields) <*> sm = MkStructUnion name (map (<*> sm) fFields)

instance Monad StructureMember where
    (MkStructField _ ty) >>= f = f ty
    (MkStructStruct name fields) >>= f = MkStructStruct name (map (>>= f) fields)
    (MkStructUnion name fields) >>= f = MkStructUnion name (map (>>= f) fields)
