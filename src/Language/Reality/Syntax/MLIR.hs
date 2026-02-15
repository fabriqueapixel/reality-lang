module Language.Reality.Syntax.MLIR (
    Expression (..),
    Toplevel (..),
    -- Re-exports
    module Lit,
    module Ann,
    module Ty,
) where

import Data.Map qualified as Map
import Data.Text qualified as T
import Language.Reality.Syntax.Internal.Annotation as Ann
import Language.Reality.Syntax.Internal.Literal as Lit
import Language.Reality.Syntax.Internal.Type as Ty
import Prelude hiding (Type)
import Language.Reality.Syntax.HLIR qualified as HLIR

-- | MLIR TYPE
-- | MLIR is a low-level intermediate representation (IR) designed to be a
-- | common IR for multiple high-level languages. It is designed to be
-- | extensible and to support multiple levels of abstraction.
data Expression
    = MkExprVariable Text
    | MkExprSpecialVariable Text
    | MkExprLiteral Lit.Literal
    | MkExprApplication Expression [Expression]
    | MkExprCondition Expression Expression Expression
    | MkExprSingleIf Expression Expression
    | MkExprLet Text Ty.Type (Maybe Expression)
    | MkExprStructureAccess Expression Text
    | MkExprStructureCreation (Map.Map Text (Expression, Ty.Type))
    | MkExprDereference Expression
    | MkExprReference Expression
    | MkExprUpdate Expression Expression
    | MkExprSizeOf Ty.Type
    | MkExprCast Ty.Type Expression
    | MkExprBlock [Expression]
    | MkExprWhile Expression Expression
    | MkExprReturn Expression
    | MkExprBreak
    | MkExprContinue
    deriving (Eq, Ord, Show, Generic)

data Toplevel
    = MkTopFunction Text [Ann.Annotation Ty.Type] Ty.Type [Expression]
    | MkTopExternalFunction Text [Text] [Ty.Type] Ty.Type
    | MkTopExternalVariable Text Ty.Type
    | MkTopTypeAlias Text Ty.Type
    | MkTopGlobal Text Ty.Type (Maybe Expression)
    | MkTopStructure Text [HLIR.StructureMember Ty.Type]
    | MkTopPublic Toplevel
    deriving (Eq, Ord, Show, Generic)

instance ToText Expression where
    toText (MkExprVariable n) = n
    toText (MkExprLiteral l) = toText l
    toText (MkExprApplication f args) =
        T.concat [toText f, "(", T.intercalate ", " (map toText args), ")"]
    toText (MkExprCondition cond thenBr elseBr) =
        T.concat
            [ "if "
            , toText cond
            , " then "
            , toText thenBr
            , " else "
            , toText elseBr
            ]
    toText (MkExprLet n ty Nothing) =
        T.concat ["let ", n, ": ", toText ty]
    toText (MkExprLet n ty (Just val)) =
        T.concat ["let ", n, ": ", toText ty, " = ", toText val]
    toText (MkExprStructureAccess struct field) =
        T.concat [toText struct, ".", field]
    toText (MkExprStructureCreation fields) =
        T.concat
            [ "{ "
            , T.intercalate
                ", "
                [ T.concat [field, ": ", toText expr]
                | (field, (expr, _)) <- Map.toList fields
                ]
            , " }"
            ]
    toText (MkExprDereference expr) =
        T.concat ["deref(", toText expr, ")"]
    toText (MkExprReference expr) =
        T.concat ["ref(", toText expr, ")"]
    toText (MkExprUpdate update val) =
        T.concat ["update(", toText update, ", ", toText val, ")"]
    toText (MkExprSizeOf ty) =
        T.concat ["sizeof(", toText ty, ")"]
    toText (MkExprCast ty expr) =
        T.concat ["cast(", toText expr, " : ", toText ty, ")"]
    toText (MkExprBlock exprs) =
        T.concat
            [ "{\n"
            , T.intercalate "\n" (map (\e -> "  " <> toText e <> ";") exprs)
            , "\n}"
            ]
    toText (MkExprSingleIf cond thenBr) =
        T.concat ["if ", toText cond, " then ", toText thenBr]
    toText (MkExprWhile cond body) =
        T.concat
            [ "while "
            , toText cond
            , " {\n  "
            , toText body
            , "\n}"
            ]
    toText (MkExprReturn expr) =
        T.concat ["return ", toText expr]
    toText MkExprBreak = "break"
    toText MkExprContinue = "continue"
    toText (MkExprSpecialVariable n) = n

instance ToText Toplevel where
    toText (MkTopFunction name params ret body) =
        T.concat
            [ "func "
            , name
            , "("
            , T.intercalate
                ", "
                [ toText param
                | param <- params
                ]
            , ") -> "
            , toText ret
            , " {\n"
            , T.intercalate "\n" (map (\e -> "  " <> toText e <> ";") body)
            , "\n}"
            ]
    toText (MkTopExternalFunction name generics args ret) =
        T.concat
            [ "extern func "
            , name
            , "["
            , T.intercalate
                ", "
                generics
            , "]"
            , "("
            , T.intercalate
                ", "
                [ toText arg
                | arg <- args
                ]
            , ") -> "
            , toText ret
            ]
    toText (MkTopGlobal name ty Nothing) =
        T.concat ["global ", name, ": ", toText ty]
    toText (MkTopGlobal name ty (Just val)) =
        T.concat ["global ", name, ": ", toText ty, " = ", toText val]
    toText (MkTopStructure name fields) =
        T.concat
            [ "struct "
            , name
            , " {\n"
            , T.intercalate
                "\n"
                (map toText fields)
            , "\n}"
            ]
    toText (MkTopPublic node) = T.concat ["public ", toText node]
    toText (MkTopExternalVariable name ty) =
        T.concat ["extern var ", name, ": ", toText ty]
    toText (MkTopTypeAlias name ty) =
        T.concat ["typealias ", name, " = ", toText ty]
