module Language.Reality.Backend.Codegen where

import Control.Monad.Result qualified as Err
import Data.Char qualified as Char
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Syntax.MLIR qualified as MLIR
import qualified Language.Reality.Syntax.HLIR as HLIR
import qualified Text.Printf as Text
import Language.Reality.Backend.Specialization.Resolver (getAllFields)

typedefs :: IORef [Text]
typedefs = IO.unsafePerformIO $ newIORef []
{-# NOINLINE typedefs #-}

symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO $ newIORef 0
{-# NOINLINE symbolCounter #-}

userDefinedTypes :: IORef (Set Text)
userDefinedTypes = IO.unsafePerformIO $ newIORef Set.empty
{-# NOINLINE userDefinedTypes #-}

types :: IORef [(MLIR.Type, Text)]
types = IO.unsafePerformIO $ newIORef []
{-# NOINLINE types #-}

getUserDefinedTypes :: [MLIR.Toplevel] -> Set Text
getUserDefinedTypes (MLIR.MkTopStructure name _ : xs) =
    Set.insert (varify name) (getUserDefinedTypes xs)
getUserDefinedTypes (_ : xs) = getUserDefinedTypes xs
getUserDefinedTypes [] = Set.empty

-- | CODEGEN
-- | Convert MLIR to a C code string.
codegenProgram :: (MonadIO m) => [MLIR.Toplevel] -> m Text
codegenProgram toplevels = do
    let userTypes = getUserDefinedTypes toplevels

    writeIORef userDefinedTypes userTypes
    writeIORef typedefs []
    writeIORef types []

    codeLines <- mapM codegenToplevel toplevels

    typedefs' <- readIORef typedefs

    pure (Text.unlines (typedefs' <> codeLines))

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = case unsnoc xs of
    Just (init', last') -> Just (x : init', last')
    Nothing -> Nothing

-- | Convert a single MLIR toplevel node to C code lines.
-- | This function takes a toplevel node, and returns a list of C code lines.
-- | This is a simplified example and does not cover all MLIR constructs.
-- | You would need to expand this function to handle all the constructs you need.
codegenToplevel :: (MonadIO m) => MLIR.Toplevel -> m Text
codegenToplevel (MLIR.MkTopFunction name params ret body) = do
    paramList <-
        Text.intercalate ", "
            <$> mapM
                ( \(MLIR.MkAnnotation paramName ty) -> codegenType True False (Just (varify paramName)) [] ty
                )
                params

    retType <- codegenType True False Nothing [] ret
    let funcHeader = Text.concat [retType, " ", varify name, "(", paramList, ") {"]
    funcBody <- mapM codegenExpression body
    let funcFooter = ["}"]
    let insertedReturn = case (unsnoc funcBody, unsnoc body) of
            (Just (initLines, lastLine), Just (_, MLIR.MkExprReturn _)) ->
                map (<> ";") initLines <> [Text.concat ["    ", lastLine, ";"]]
            (Just (initLines, lastLine), Just (_, MLIR.MkExprVariable "")) ->
                map (<> ";") initLines <> [Text.concat ["    ", lastLine, ";"]]
            (Just (initLines, lastLine), Just (_, expr)) | containsSpecialVariable expr ->
                map (<> ";") initLines <> [Text.concat ["    ", lastLine, ";"]]
            (Just (initLines, lastLine), _) ->
                map (<> ";") initLines <> [Text.concat ["    return ", lastLine, ";"]]
            (Nothing, _) -> []

    modifyIORef' typedefs (<> [retType <> " " <> varify name <> "(" <> paramList <> ");"])

    pure $ Text.concat ([funcHeader] ++ insertedReturn ++ funcFooter)
codegenToplevel (MLIR.MkTopGlobal name ty (Just expr)) = do
    constType <- codegenType True False (Just name) [] ty
    constValue <- codegenExpression expr
    pure $ Text.concat [constType, " = ", constValue, ";"]
codegenToplevel (MLIR.MkTopGlobal name ty Nothing) = do
    constType <- codegenType True False (Just name) [] ty
    pure $ Text.concat [constType, ";"]
codegenToplevel (MLIR.MkTopPublic n) = do
    innerCode <- codegenToplevel n
    pure $ Text.concat ["// Public\n", innerCode]
codegenToplevel (MLIR.MkTopStructure name fields) = do
    modifyIORef' userDefinedTypes (Set.insert (varify name))
    fieldLines <- mapM codegenField fields
    let structHeader = Text.concat ["struct ", varify name, " {"]
    let structFooter = Text.concat ["};"]
    pure $ Text.unlines ([structHeader] ++ fieldLines ++ [structFooter])
codegenToplevel (MLIR.MkTopExternalFunction name generics params ret) = do
    paramList <-
        Text.intercalate ", "
            <$> mapM (codegenType True True Nothing generics) params
    retType <- codegenType True True (Just name) generics ret
    pure $ Text.concat ["extern ", retType, "(", paramList, ");"]
codegenToplevel (MLIR.MkTopExternalVariable name ty) = do
    varType <- codegenType True True (Just name) [] ty
    pure $ Text.concat ["extern ", varType, ";"]
codegenToplevel (MLIR.MkTopTypeAlias name ty) = do
    modifyIORef' userDefinedTypes (Set.insert (varify name))
    tyStr <- codegenType True False Nothing [] ty
    pure $ Text.concat ["typedef ", tyStr, " ", varify name, ";"]

codegenField :: (MonadIO m) => HLIR.StructureMember HLIR.Type -> m Text
codegenField (HLIR.MkStructField name ty) = do
    tyStr <- codegenType False False (Just (varify name)) [] ty
    pure $ Text.concat [tyStr, ";"]
codegenField (HLIR.MkStructStruct name fields) = do
    fieldLines <- mapM codegenField fields
    let structHeader = "struct {"
    let structFooter = "} " <> varify name <> ";"
    pure
        $ Text.unlines ([structHeader] ++ map ("    " <>) fieldLines ++ [structFooter])
codegenField (HLIR.MkStructUnion name fields) = do
    fieldLines <- mapM codegenField fields
    let unionHeader = "union {"
    let unionFooter = "} " <> varify name <> ";"
    pure
        $ Text.unlines ([unionHeader] ++ map ("    " <>) fieldLines ++ [unionFooter])

containsSpecialVariable :: MLIR.Expression -> Bool
containsSpecialVariable (MLIR.MkExprSpecialVariable _) = True
containsSpecialVariable (MLIR.MkExprApplication f args) =
    containsSpecialVariable f || any containsSpecialVariable args
containsSpecialVariable (MLIR.MkExprBlock exprs) =
    any containsSpecialVariable exprs
containsSpecialVariable (MLIR.MkExprCondition cond thenBr elseBr) =
    containsSpecialVariable cond
        || containsSpecialVariable thenBr
        || containsSpecialVariable elseBr
containsSpecialVariable (MLIR.MkExprLet _ _ (Just expr)) =
    containsSpecialVariable expr
containsSpecialVariable (MLIR.MkExprLet _ _ Nothing) = False
containsSpecialVariable (MLIR.MkExprDereference e) = containsSpecialVariable e
containsSpecialVariable (MLIR.MkExprReference e) = containsSpecialVariable e
containsSpecialVariable (MLIR.MkExprSizeOf _) = False
containsSpecialVariable (MLIR.MkExprStructureAccess struct _) =
    containsSpecialVariable struct
containsSpecialVariable (MLIR.MkExprStructureCreation fields) =
    any (containsSpecialVariable . fst) (Map.elems fields)
containsSpecialVariable (MLIR.MkExprUpdate e v) =
    containsSpecialVariable e || containsSpecialVariable v
containsSpecialVariable (MLIR.MkExprSingleIf cond thenBr) =
    containsSpecialVariable cond || containsSpecialVariable thenBr
containsSpecialVariable (MLIR.MkExprWhile cond body) =
    containsSpecialVariable cond || containsSpecialVariable body
containsSpecialVariable (MLIR.MkExprReturn e) = containsSpecialVariable e
containsSpecialVariable _ = False

findType :: [(MLIR.Type, Text)] -> Map Text (MLIR.Expression, MLIR.Type) -> Maybe Text
findType ((HLIR.MkTyRecord r, n) : xs) structMap = do
    let (fields, _) = getAllFields r

    if fields == Map.map snd structMap
        then Just n
        else findType xs structMap
findType (_ : xs) structMap = findType xs structMap
findType [] _ = Nothing

-- | Convert a single MLIR expression to C code lines.
-- | This function takes an expression, and returns a list of C code lines.
-- | This is a simplified example and does not cover all MLIR constructs.
-- | You would need to expand this function to handle all the constructs you need.
codegenExpression :: (MonadIO m) => MLIR.Expression -> m Text
codegenExpression (MLIR.MkExprLiteral lit) = codegenLiteral lit
codegenExpression (MLIR.MkExprVariable name) = pure $ varify name
codegenExpression (MLIR.MkExprApplication (MLIR.MkExprVariable "or") args) = do
    argList <- Text.intercalate " || " <$> mapM codegenExpression args
    pure $ Text.concat ["(", argList, ")"]
codegenExpression (MLIR.MkExprApplication (MLIR.MkExprVariable "||") args) = do
    argList <- Text.intercalate " || " <$> mapM codegenExpression args
    pure $ Text.concat ["(", argList, ")"]
codegenExpression (MLIR.MkExprApplication (MLIR.MkExprVariable "and") args) = do
    argList <- Text.intercalate " && " <$> mapM codegenExpression args
    pure $ Text.concat ["(", argList, ")"]
codegenExpression (MLIR.MkExprApplication (MLIR.MkExprVariable "&&") args) = do
    argList <- Text.intercalate " && " <$> mapM codegenExpression args
    pure $ Text.concat ["(", argList, ")"]
codegenExpression (MLIR.MkExprApplication f args) = do
    fStr <- codegenExpression f
    argList <- Text.intercalate ", " <$> mapM codegenExpression args
    pure $ Text.concat [fStr, "(", argList, ")"]
codegenExpression (MLIR.MkExprBlock bl) = do
    blockLines <- mapM codegenExpression bl
    pure $ Text.concat ["{", Text.concat (map (<> ";") blockLines), "}"]
codegenExpression (MLIR.MkExprCast ty expr) = do
    tyStr <- codegenType True False Nothing [] ty
    exprStr <- codegenExpression expr
    pure $ Text.concat ["(", tyStr, ")", exprStr]
codegenExpression (MLIR.MkExprCondition cond thenBr elseBr) = do
    condExpr <- codegenExpression cond
    thenExpr <- codegenExpression thenBr
    elseExpr <- codegenExpression elseBr
    pure $ Text.concat ["if (", condExpr, ") ", thenExpr, " else ", elseExpr]
codegenExpression (MLIR.MkExprLet name ty (Just expr)) = do
    varType <- codegenType True False (Just name) [] ty
    varValue <- codegenExpression expr
    pure $ Text.concat [varType, " = ", varValue, ";"]
codegenExpression (MLIR.MkExprLet name ty Nothing) = do
    varType <- codegenType True False (Just name) [] ty
    pure $ Text.concat [varType, ";"]
codegenExpression (MLIR.MkExprDereference e) = do
    eStr <- codegenExpression e
    pure $ Text.concat ["(*", eStr, ")"]
codegenExpression (MLIR.MkExprReference e) = do
    eStr <- codegenExpression e
    pure $ Text.concat ["(&", eStr, ")"]
codegenExpression (MLIR.MkExprSizeOf ty) = do
    tyStr <- codegenType True False Nothing [] ty
    pure $ Text.concat ["sizeof(", tyStr, ")"]
codegenExpression (MLIR.MkExprStructureAccess (MLIR.MkExprDereference e) f) = do
    eStr <- codegenExpression e
    pure $ Text.concat ["(", eStr, ")->", varify f]
codegenExpression (MLIR.MkExprStructureAccess e f) = do
    eStr <- codegenExpression e
    pure $ Text.concat [eStr, ".", varify f]
codegenExpression (MLIR.MkExprStructureCreation fields) = do
    types' <- readIORef types

    let result = findType types' fields

    let structType = maybe "" (\n -> Text.concat ["(struct ", n, ")"]) result

    fieldInits <-
        mapM
            ( \(n, (v, _)) -> do
                vStr <- codegenExpression v
                pure $ Text.concat [".", n, " = ", vStr]
            )
            (Map.toList fields)
    pure $ Text.concat [structType, " { ", Text.intercalate ", " fieldInits, " }"]
codegenExpression (MLIR.MkExprUpdate e v) = do
    eStr <- codegenExpression e
    vStr <- codegenExpression v
    pure $ Text.concat [eStr, " = ", vStr]
codegenExpression (MLIR.MkExprSingleIf cond thenBranch) = do
    condExpr <- codegenExpression cond
    thenExpr <- codegenExpression thenBranch
    pure $ Text.concat ["if (", condExpr, ") ", thenExpr]
codegenExpression (MLIR.MkExprWhile cond body) = do
    condExpr <- codegenExpression cond
    bodyExpr <- codegenExpression body
    pure $ Text.concat ["while (", condExpr, ") { ", bodyExpr, " } "]
codegenExpression (MLIR.MkExprReturn e) = do
    eStr <- codegenExpression e
    pure $ Text.concat ["return ", eStr]
codegenExpression MLIR.MkExprBreak = pure "break"
codegenExpression MLIR.MkExprContinue = pure "continue"
codegenExpression (MLIR.MkExprSpecialVariable n) = pure ("/* SPECIAL */ " <> varify n)

-- | Convert a single MLIR literal to a C code string.
-- | This function takes a literal, and returns a C code string.
-- | This is a simplified example and does not cover all MLIR literal types.
-- | You would need to expand this function to handle all the literal types you need.
codegenLiteral :: (MonadIO m) => MLIR.Literal -> m Text
codegenLiteral (MLIR.MkLitInt n) = pure $ Text.pack (show n)
codegenLiteral (MLIR.MkLitFloat f) = pure $ Text.pack (show f)
codegenLiteral (MLIR.MkLitBool True) = pure "true"
codegenLiteral (MLIR.MkLitBool False) = pure "false"
codegenLiteral (MLIR.MkLitString s) =
    pure $ Text.concat ["\"", encodeUnicode16 s, "\""]
codegenLiteral (MLIR.MkLitChar c) = do
    let cShow = show (Text.singleton c)
    inner <- dropDoubleQuotes (Text.pack cShow)
    pure $ Text.concat ["'", inner, "'"]

dropDoubleQuotes :: (MonadIO m) => Text -> m Text
dropDoubleQuotes txt =
    if Text.length txt >= 2 && Text.head txt == '\"' && Text.last txt == '\"'
        then pure $ Text.init (Text.tail txt)
        else pure txt

-- | Convert a single MLIR type to a C type string.
-- | This function takes a type, and returns a C type string.
-- | This is a simplified example and does not cover all MLIR type constructs.
-- | You would need to expand this function to handle all the type constructs you need.
codegenType ::
    (MonadIO m) => Bool -> Bool -> Maybe Text -> [Text] -> MLIR.Type -> m Text
codegenType _ _ def _ (MLIR.MkTyId "int") = pure $ Text.concat ["int32_t ", fromMaybe "" def]
codegenType _ _ def _ (MLIR.MkTyId "float") = pure $ Text.concat ["float ", fromMaybe "" def]
codegenType _ _ def _ (MLIR.MkTyId "char") = pure $ Text.concat ["char ", fromMaybe "" def]
codegenType _ _ def _ (MLIR.MkTyId "bool") = pure $ Text.concat ["bool ", fromMaybe "" def]
codegenType _ _ def _ (MLIR.MkTyId "void") = pure $ Text.concat ["void ", fromMaybe "" def]
codegenType _ _ def _ (HLIR.MkTyId "never") = pure $ Text.concat ["void ", fromMaybe "" def]
codegenType _ _ def generics (MLIR.MkTyId n) | n `elem` generics = pure $ Text.concat ["void*", fromMaybe "" def]
codegenType _ _ def _ (MLIR.MkTyId n) = do
    let typeName = varify n

    typedefs' <- readIORef typedefs

    let typedef = Text.concat ["struct ", typeName, ";"]

    userTypes <- readIORef userDefinedTypes

    when (typeName `Set.member` userTypes && typedef `notElem` typedefs') $ do
        modifyIORef' typedefs (<> [typedef])

    pure
        $ Text.concat
            [ if typeName `Set.member` userTypes then "struct " else ""
            , typeName
            , " "
            , fromMaybe "" def
            ]
codegenType shouldPutEnv ext def _ (MLIR.MkTyVar tv) = do
    let tvr = IO.unsafePerformIO $ readIORef tv

    case tvr of
        MLIR.Link ty -> codegenType shouldPutEnv ext def [] ty
        MLIR.Unbound name _ -> Err.compilerError $ "Unbound type variable in codegen: " <> name
codegenType shouldPutEnv ext def generics ptr@(MLIR.MkTyPointer ty)
    | (MLIR.MkTyFun args ret, n) <- getMultipleTimesPointer ptr = do
        argTypes <-
            Text.intercalate ", " <$> mapM (codegenType True ext Nothing generics) args
        retType <- codegenType shouldPutEnv ext Nothing generics ret

        name <- freshSymbol

        let typedefLine =
                Text.concat
                    ["typedef ", retType, " (*", Text.replicate n "*", name, ")(", argTypes, ");"]

        modifyIORef' typedefs (<> [typedefLine])

        pure $ Text.concat [name, " ", fromMaybe "" def]
    | otherwise = do
        innerTy <- codegenType shouldPutEnv ext Nothing generics ty
        pure $ Text.concat [innerTy, "*", fromMaybe "" def]
codegenType _ True def _ (HLIR.MkTyFun {}) = pure $ Text.concat ["void* ", fromMaybe "" def]
codegenType shouldPutEnv ext def generics (args MLIR.:->: ret) = do
    argTypes <-
        Text.intercalate ", " <$> mapM (codegenType True ext Nothing generics) args
    retType <- codegenType shouldPutEnv ext Nothing generics ret

    name <- freshSymbol

    let typedefLine = Text.concat ["typedef ", retType, " (*", name, ")(", argTypes, ");"]

    modifyIORef' typedefs (<> [typedefLine])

    pure $ Text.concat [name, " ", fromMaybe "" def]
codegenType shouldPutEnv ext def generics (HLIR.MkTyClosure env fun) = do
    envType <- codegenType shouldPutEnv ext Nothing generics env
    funType <- codegenType shouldPutEnv ext Nothing generics fun

    types' <- readIORef types
    let found = findTypeByType types' (buildTypeWithRest [("environment", HLIR.MkTyPointer env), ("function", fun)] HLIR.MkTyRowEmpty)

    case found of
        Just typeName -> pure $ Text.concat [typeName, " ", fromMaybe "" def]
        Nothing -> do
            name <- ("_closure_" <>) <$> freshSymbol

            let structDef = Text.concat ["typedef struct ", name, " { ", envType, " environment; ", funType, " function; } ", name, ";"]

            modifyIORef' typedefs (<> [structDef])
            modifyIORef' types (<> [(buildTypeWithRest [("environment", env), ("function", fun)] HLIR.MkTyRowEmpty, name)])

            pure $ Text.concat ["struct ", name, " ", fromMaybe "" def]
codegenType shouldPutEnv ext def generics (HLIR.MkTyRecord recTy) = do
    let ty = removeDuplicateFields (HLIR.MkTyRecord recTy)
    types' <- readIORef types

    let found = findTypeByType types' ty

    case found of
        Just typeName -> pure $ Text.concat [typeName, " ", fromMaybe "" def]
        Nothing -> do
            ty' <- case ty of
                HLIR.MkTyRecord r -> 
                    codegenType shouldPutEnv ext def generics r
                _ -> codegenType shouldPutEnv ext def generics ty

            name <- ("_record_" <>) <$> freshSymbol

            let structDef = Text.concat ["typedef struct ", name, " { ", ty', "} ", name, ";"]

            modifyIORef' typedefs (<> [structDef])
            modifyIORef' types (<> [(ty, name)])

            pure $ Text.concat ["struct ", name, " ", fromMaybe "" def]
codegenType _ _ def _ (MLIR.MkTyQuantified _) = pure $ Text.concat ["void* ", fromMaybe "" def]
codegenType _ _ _ _ t@(MLIR.MkTyApp _ _) =
    Err.compilerError
        $ "Type applications are not directly supported in C codegen: "
            <> Text.pack (show t)
codegenType shouldPutEnv ext _ generics (MLIR.MkTyRowExtend field ty rest) = do
    fieldTy <- codegenType shouldPutEnv ext Nothing generics ty
    restTy <- codegenType shouldPutEnv ext Nothing generics rest
    let structDef = Text.concat [fieldTy, " ", field, "; ", restTy]
    pure structDef
codegenType _ _ _ _ MLIR.MkTyRowEmpty = pure ""

findTypeByType :: [(MLIR.Type, Text)] -> MLIR.Type -> Maybe Text
findTypeByType ((t, n) : xs) (MLIR.MkTyRecord r) =
    let (fields, _) = getAllFields r
     in case t of
            HLIR.MkTyRecord r' ->
                let (fields', _) = getAllFields r'
                 in if fields == fields'
                        then Just n
                        else findTypeByType xs (MLIR.MkTyRecord r)
            _ -> findTypeByType xs (MLIR.MkTyRecord r)
findTypeByType (_ : xs) ty = findTypeByType xs ty
findTypeByType [] _ = Nothing

isLambdaEnv :: MLIR.Type -> Bool
isLambdaEnv (MLIR.MkTyId n) | "closure" `Text.isPrefixOf` n = True
isLambdaEnv (MLIR.MkTyPointer ty) = isLambdaEnv ty
isLambdaEnv _ = False

isIdent :: Char -> Bool
isIdent x = Char.isAlphaNum x || x == '_' || x == '$'

varify :: Text -> Text
varify "" = ""
varify n | n `Set.member` cKeywords = Text.concat ["_", n]
varify n =
    Text.concatMap
        (\x' -> if isIdent x' then toText [x'] else fromString (show (ord x')))
        n

cKeywords :: Set Text
cKeywords =
    Set.fromList
        [ "auto"
        , "break"
        , "case"
        , "char"
        , "const"
        , "continue"
        , "default"
        , "do"
        , "double"
        , "else"
        , "enum"
        , "extern"
        , "float"
        , "for"
        , "goto"
        , "if"
        , "inline"
        , "int"
        , "long"
        , "register"
        , "restrict"
        , "return"
        , "short"
        , "signed"
        , "sizeof"
        , "static"
        , "struct"
        , "switch"
        , "typedef"
        , "union"
        , "unsigned"
        , "void"
        , "volatile"
        , "while"
        , "_Alignas"
        , "_Alignof"
        , "_Atomic"
        , "_Bool"
        , "_Complex"
        , "_Generic"
        , "_Imaginary"
        , "_Noreturn"
        , "_Static_assert"
        , "_Thread_local"
        ]

getMultipleTimesPointer :: MLIR.Type -> (MLIR.Type, Int)
getMultipleTimesPointer (MLIR.MkTyPointer ty) =
    let (baseTy, count) = getMultipleTimesPointer ty
     in (baseTy, count + 1)
getMultipleTimesPointer ty = (ty, 0)

encodeUnicode16 :: Text -> Text
encodeUnicode16 = Text.concatMap escapeChar
  where
    escapeChar c
        | c == '"' = Text.pack "\\\""
        | ' ' <= c && c <= 'z' = Text.singleton c
        | Char.isPrint c = Text.singleton c
        | Char.isControl c = Text.pack $ Text.printf "\\x%02x" (fromEnum c)
        | otherwise =
            Text.pack $ Text.printf "\\u%04x" (fromEnum c)

freshSymbol :: (MonadIO m) => m Text
freshSymbol = do
    idx <- atomicModifyIORef' symbolCounter (\i -> (i + 1, i))
    pure $ Text.pack ("_gen" <> show idx)

removeDuplicateFields :: MLIR.Type -> MLIR.Type
removeDuplicateFields (MLIR.MkTyRecord r) =
    let (fields, rest) = getAllFields r
     in HLIR.MkTyRecord (buildTypeWithRest (Map.toList fields) (removeDuplicateFields rest))
removeDuplicateFields (MLIR.MkTyRowExtend label ty rest) =
    HLIR.MkTyRowExtend label (removeDuplicateFields ty) (removeDuplicateFields rest)
removeDuplicateFields t = t

buildTypeWithRest :: [(Text, MLIR.Type)] -> MLIR.Type -> MLIR.Type
buildTypeWithRest [] rest = rest
buildTypeWithRest ((label, fieldType) : xs) rest =
    HLIR.MkTyRowExtend label fieldType (buildTypeWithRest xs rest)
