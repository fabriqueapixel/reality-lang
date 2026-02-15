module Language.Reality.Backend.ANF.Converter where

import Control.Monad.Result qualified as Err
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Syntax.HLIR qualified as HLIR
import Language.Reality.Syntax.MLIR qualified as MLIR
import qualified Data.List as List
import qualified Data.Set as Set

isReturnExpr :: HLIR.TLIR "expression" -> Bool
isReturnExpr (HLIR.MkExprReturn _) = True
isReturnExpr (HLIR.MkExprLocated _ e) = isReturnExpr e
isReturnExpr _ = False

mapAndUnzip3M :: (Monad m) => (a -> m (b, c, d)) -> [a] -> m ([b], [c], [d])
mapAndUnzip3M f xs = do
    (bs, cs, ds) <- unzip3 <$> mapM f xs
    pure (bs, cs, ds)

unitExpr :: HLIR.TLIR "expression"
unitExpr = HLIR.MkExprVariable (HLIR.MkAnnotation "unit" (Identity (HLIR.MkTyId "unit"))) []

variablesToDeclare :: IORef [(Text, [MLIR.Expression])]
variablesToDeclare = IO.unsafePerformIO $ newIORef []
{-# NOINLINE variablesToDeclare #-}

-- | ANF CONVERTER
-- | Convert a HLIR expression to ANF (MLIR).
-- | This function takes a HLIR expression, and returns a MLIR expression.
-- | The conversion is done by introducing let bindings for non-trivial expressions.
-- | Trivial expressions are:
-- | - Variables
-- | - Constants
-- | - Function applications
-- | - Structure access
-- | - SizeOf
-- | Non-trivial expressions are:
-- | - Let expressions
-- | - Conditionals
-- | - Structure creation
-- | - Dereference
-- | - Reference
-- | - Update
-- | The conversion is done recursively, so that all non-trivial expressions are
-- | converted to let bindings.
convertToANF ::
    (MonadIO m) =>
    [HLIR.TLIR "toplevel"] ->
    m [MLIR.Toplevel]
convertToANF xs = do
    toplevels <- mapM convertToplevel xs

    pure ([MLIR.MkTopTypeAlias "never" (HLIR.MkTyId "void")] <> concat toplevels)

-- | Convert a HLIR toplevel to a MLIR toplevel.
-- | This function takes a HLIR toplevel, and returns a MLIR toplevel.
-- | The conversion is done by converting the body of the toplevel.
convertToplevel :: (MonadIO m) => HLIR.TLIR "toplevel" -> m [MLIR.Toplevel]
convertToplevel (HLIR.MkTopExternLet ann) = do
    pure [MLIR.MkTopExternalVariable ann.name ann.typeValue]
convertToplevel (HLIR.MkTopFunctionDeclaration ann params ret body) = do
    (body', lets, bindings) <- convertExpression body

    -- If the function is "main", we need to add the GC start and end calls.
    -- We also need to add the "argc" and "argv" parameters if they are not present.
    -- We assume that "main" has the following signature:
    -- fn main(argc: int, argv: **char): int
    let gcStart =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable "GC_INIT")
                [ ]

    -- Resetting the main arguments to the expected ones if they are not correct.
    let args
            | ann.name == "main" && length params /= 2 =
                [ MLIR.MkAnnotation "argc" MLIR.MkTyInt
                , MLIR.MkAnnotation "argv" (MLIR.MkTyPointer (MLIR.MkTyPointer MLIR.MkTyChar))
                ]
            | otherwise = params

    varsToDeclare <- readIORef variablesToDeclare

    let varsUpdates = concat [
            exprs <> [MLIR.MkExprUpdate (MLIR.MkExprVariable varName) (MLIR.MkExprVariable varName)] | (varName, exprs) <- varsToDeclare ]

    -- Building the final body with GC calls if needed.
    -- If the function is not "main", we just use the body with let bindings.
    let body''
            | ann.name == "main" = gcStart : varsUpdates ++ lets ++ bindings ++ [body']
            | otherwise = lets ++ bindings ++ [body']

    pure [MLIR.MkTopFunction ann.name args ret body'']
convertToplevel (HLIR.MkTopConstantDeclaration ann expr) = do
    (expr', lets, _) <- convertExpression expr

    unless (null lets)
        $ Err.compilerError
            "Top-level constant declarations cannot contain let bindings. Please inline the expression."

    pure [MLIR.MkTopGlobal ann.name ann.typeValue (Just expr')]
convertToplevel (HLIR.MkTopLocated _ e) = convertToplevel e
convertToplevel (HLIR.MkTopModuleDeclaration{}) =
    Err.compilerError "Modules should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopImport _) =
    Err.compilerError "Imports should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopTypeAlias name aliasedType) = do
    case aliasedType of
        HLIR.MkTyRecord row -> do
            let fields = getFields row
            namedFields <- forM (Map.toList fields) $ \(name', fieldType) -> do
                pure $ HLIR.MkStructField name' fieldType

            pure [MLIR.MkTopStructure name.name namedFields]

        ty -> pure [MLIR.MkTopTypeAlias name.name ty]
convertToplevel (HLIR.MkTopPublic node) = do
    node' <- convertToplevel node
    pure $ MLIR.MkTopPublic <$> node'
convertToplevel (HLIR.MkTopExternalFunction name parameters returnType) =
    pure
        [ MLIR.MkTopExternalFunction
            name.name
            name.typeValue
            (map (.typeValue) parameters)
            returnType
        ]
convertToplevel (HLIR.MkTopImplementation{}) =
    Err.compilerError
        "Implementations should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopProperty{}) =
    Err.compilerError "Properties should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopAnnotation{}) =
    Err.compilerError "Annotations should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopEnumeration header constructors) = do
    enum <- createEnumStructure (header.name, constructors)
    functions <- mapM (createFunction header.name) (Map.toList constructors)

    pure (enum : functions)
  where
    createEnumStructure ::
        (MonadIO m) => (Text, Map Text (Maybe [MLIR.Type])) -> m MLIR.Toplevel
    createEnumStructure (enumName, constructors') = do
        let fields = Map.map (fromMaybe []) constructors'

        namedFields <- forM (Map.toList fields) $ \(name, fieldTypes) -> do
            fieldNames <- forM (zip fieldTypes [(1 :: Int) ..]) $ \(ty, idx) ->
                pure (HLIR.MkStructField (name <> "_field" <> Text.pack (show idx)) ty)
            pure $ HLIR.MkStructStruct name fieldNames

        let enumStructure =
                MLIR.MkTopStructure
                    enumName
                    [ HLIR.MkStructField "_tag" MLIR.MkTyString
                    , HLIR.MkStructUnion "_data" namedFields
                    ]

        pure enumStructure

    createFunction ::
        (MonadIO m) => Text -> (Text, Maybe [MLIR.Type]) -> m MLIR.Toplevel
    createFunction enumName (consName, maybeFieldTypes) = do
        let arguments = case maybeFieldTypes of
                Just fieldTypes ->
                    zipWith
                        (\ty idx -> MLIR.MkAnnotation ("arg" <> Text.pack (show idx)) ty)
                        fieldTypes
                        [(1 :: Int) ..]
                Nothing -> []

            structType = MLIR.MkTyId enumName

            bodyExpr =
                [ MLIR.MkExprLet "result" structType Nothing
                , MLIR.MkExprUpdate
                    (MLIR.MkExprStructureAccess (MLIR.MkExprVariable "result") "_tag")
                    (MLIR.MkExprLiteral (MLIR.MkLitString consName))
                ]
                    <> [ MLIR.MkExprUpdate
                            ( MLIR.MkExprStructureAccess
                                ( MLIR.MkExprStructureAccess
                                    (MLIR.MkExprStructureAccess (MLIR.MkExprVariable "result") "_data")
                                    consName
                                )
                                (consName <> "_field" <> Text.pack (show idx))
                            )
                            (MLIR.MkExprVariable arg.name)
                       | (arg, idx) <- zip arguments [(1 :: Int) ..]
                       ]
                    ++ [MLIR.MkExprVariable "result"]

        pure
            $ MLIR.MkTopFunction
                consName
                arguments
                structType
                bodyExpr
convertToplevel (HLIR.MkTopLet binding value) = do
    (value', lets, _) <- convertExpression value

    modifyIORef' variablesToDeclare $ \vars ->
        vars ++ [(binding.name, lets ++ [value'])]

    pure [MLIR.MkTopGlobal binding.name binding.typeValue.runIdentity Nothing]

isCFStatement :: HLIR.TLIR "expression" -> Bool
isCFStatement (HLIR.MkExprReturn _) = True
isCFStatement HLIR.MkExprBreak = True
isCFStatement HLIR.MkExprContinue = True
isCFStatement (HLIR.MkExprLocated _ e) = isCFStatement e
isCFStatement (HLIR.MkExprWhile _ _ _ inExpr) = isCFStatement inExpr
isCFStatement (HLIR.MkExprLetIn _ _ inExpr _) = isCFStatement inExpr
isCFStatement (HLIR.MkExprCondition _ thenB elseB _ _) =
    isCFStatement thenB || isCFStatement elseB
isCFStatement (HLIR.MkExprSingleIf _ thenB _) = isCFStatement thenB
isCFStatement _ = False

getIs :: HLIR.TLIR "expression" -> Maybe (HLIR.TLIR "expression", HLIR.TLIR "pattern", HLIR.Type)
getIs (HLIR.MkExprIs expr pat exprTy) = Just (expr, pat, exprTy.runIdentity)
getIs (HLIR.MkExprLocated _ e) = getIs e
getIs _ = Nothing

isVariable :: HLIR.TLIR "expression" -> Text -> Bool
isVariable (HLIR.MkExprVariable ann _) name = ann.name == name
isVariable (HLIR.MkExprLocated _ e) name = isVariable e name
isVariable _ _ = False

getConditions :: HLIR.TLIR "expression" -> [HLIR.TLIR "expression"]
getConditions (HLIR.MkExprCondition cond thenB elseB _ _) =
    cond : (getConditions thenB ++ getConditions elseB)
getConditions (HLIR.MkExprLocated _ e) = getConditions e
getConditions (HLIR.MkExprApplication callee args t)
    | isVariable callee "and" = do
        let conds = concatMap getConditions args
        conds
    | otherwise =
        [HLIR.MkExprApplication
            callee
            (concatMap getConditions args)
            t]
getConditions e = [e]

createCondition ::
    MonadIO m =>
    [HLIR.TLIR "expression"] ->
    HLIR.TLIR "expression" ->
    HLIR.TLIR "expression" ->
    (Bool, Bool) ->
    Text ->
    m [MLIR.Expression]
createCondition [] thenB _ (su1, _)  n = do
    (thenB', l3, bs3) <- convertExpression thenB

    let finalExpr
            | not su1 && n /= "" =
                let update = MLIR.MkExprUpdate (MLIR.MkExprVariable n) thenB'
                in MLIR.MkExprBlock $ l3 ++ bs3 ++ [update]
            | otherwise = MLIR.MkExprBlock $ l3 ++ bs3 ++ [thenB']
    pure [finalExpr]
createCondition (cond' : rest) t elseB (su1, su2) n = do
    (cond'', l1, bs1) <- convertExpression cond'

    innerCondition <- createCondition rest t elseB (su1, su2) n

    (elseB', l3, bs3) <- convertExpression elseB

    let elseExpr
            | not su2 && n /= "" =
                let update = MLIR.MkExprUpdate (MLIR.MkExprVariable n) elseB'
                in MLIR.MkExprBlock $ l3 ++ bs3 ++ [update]
            | otherwise = MLIR.MkExprBlock $ l3 ++ bs3 ++ [elseB']
        ifExpr = l1 ++ bs1 ++ [MLIR.MkExprCondition cond'' (MLIR.MkExprBlock innerCondition) elseExpr]

    pure ifExpr

isVoidType :: MonadIO m => HLIR.Type -> m Bool
isVoidType (HLIR.MkTyId "never") = pure True
isVoidType (HLIR.MkTyFun _ retType) = isVoidType retType
isVoidType (HLIR.MkTyVar ref) = do
    ty <- liftIO $ readIORef ref
    case ty of
        HLIR.Link ty' -> isVoidType ty'
        HLIR.Unbound{} -> pure False
isVoidType _ = pure False

local' :: MonadIO m => m a -> m a
local' action = do
    oldMap <- liftIO $ readIORef variableRenamingMap
    result <- action
    liftIO $ writeIORef variableRenamingMap oldMap
    pure result

buildMap :: HLIR.TLIR "expression" -> (Map Text (HLIR.TLIR "expression", HLIR.Type), HLIR.TLIR "expression")
buildMap (HLIR.MkExprLocated _ e) = buildMap e
buildMap (HLIR.MkExprStructureCreation k e r t _) = do
    let (r', rest) = buildMap r

    (Map.insert k (e, t.runIdentity) r', rest)
buildMap e = (Map.empty, e)

-- | Convert a HLIR expression to a MLIR expression in ANF.
-- | This function takes a HLIR expression, and returns a MLIR expression.
convertExpression ::
    (MonadIO m) => HLIR.TLIR "expression" -> m (MLIR.Expression, [MLIR.Expression], [MLIR.Expression])
convertExpression (HLIR.MkExprVariable ann _) = do
    isVoid <- isVoidType ann.typeValue.runIdentity

    variableRenamingMap' <- liftIO $ readIORef variableRenamingMap
    let renamedName = Map.findWithDefault ann.name ann.name variableRenamingMap'

    if isVoid then
        pure (MLIR.MkExprSpecialVariable ann.name, [], [])
    else
        pure (MLIR.MkExprVariable renamedName, [], [])
convertExpression (HLIR.MkExprLiteral l) = pure (MLIR.MkExprLiteral l, [], [])
convertExpression (HLIR.MkExprApplication f args _) = do
    (f', l1, bs1) <- convertExpression f
    (args', l2s, bs2) <- unzip3 <$> mapM convertExpression args

    let app = MLIR.MkExprApplication f' args'

    pure (app, l1 ++ concat l2s, bs1 ++ concat bs2)

-- If encountering empty let-binding (used for sequencing), ignore the name
-- and just convert the value and inExpr.
convertExpression (HLIR.MkExprLetIn (HLIR.MkAnnotation "_" _) value inExpr _) = do
    (value', l1, bs1) <- convertExpression value
    (inExpr', l2, bs2) <- convertExpression inExpr

    pure (inExpr', l1 ++ [value'] ++ l2, bs1 ++ bs2)
convertExpression (HLIR.MkExprLetIn binding value inExpr _) = do
    (value', l1, bs1) <- convertExpression value
    newVar <- freshPrefixed binding.name
    (inExpr', l2, bs2) <- local' $ do
        modifyIORef' variableRenamingMap (Map.insert binding.name newVar)
        convertExpression inExpr

    -- If the inExpr is a control flow statement (return, break, continue),
    -- we don't need to create a new variable for it, as it will not be used.
    let letBinding = MLIR.MkExprLet newVar binding.typeValue.runIdentity (Just value')
    pure (inExpr', l1 ++ [letBinding] ++ l2, bs1 ++ bs2)
convertExpression (HLIR.MkExprCondition cond thenB elseB thenTy elseTy) = do
    let conditions = getConditions cond

    let isCF = isCFStatement thenB || isCFStatement elseB

    if isCF then do
        expr <- local' $ createCondition conditions thenB elseB (isCF, isCF) ""
        -- If one of the branches is a control flow statement,
        -- we don't need to create a new variable for it, as it will not be used.

        pure (MLIR.MkExprVariable "", expr, [])
    else do
        -- Creating a fresh symbol for storing the result of the condition.
        newVariable <- freshSymbol

        -- Converting condition can be tricky because both branches need
        -- to include lets-generated variables and update the same variable.
        --
        -- This permits to have conditions as expressions as well as conditions
        -- as statements.

        isVoid1 <- isVoidType thenTy.runIdentity
        isVoid2 <- isVoidType elseTy.runIdentity

        let def
                | isVoid1 && isVoid2 = []
                | isVoid1 = [MLIR.MkExprLet newVariable elseTy.runIdentity Nothing]
                | isVoid2 = [MLIR.MkExprLet newVariable thenTy.runIdentity Nothing]
                | otherwise = [MLIR.MkExprLet newVariable thenTy.runIdentity Nothing]

        let var
                | isVoid1 && isVoid2 = MLIR.MkExprVariable ""
                | otherwise = MLIR.MkExprVariable newVariable

        ifExpr <- local' $ createCondition
            conditions
            thenB
            elseB
            (isVoid1 || isCF, isVoid2 || isCF)
            newVariable

        pure (var, def <> ifExpr, [])
convertExpression (HLIR.MkExprLocated _ e) = convertExpression e
convertExpression (HLIR.MkExprStructureAccess struct field) = do
    (struct', l1, bs1) <- convertExpression struct
    let access = MLIR.MkExprStructureAccess struct' field
    pure (access, l1, bs1)
convertExpression (HLIR.MkExprStructureCreation k e r t rt) = do
    let (creation, rest) = buildMap (HLIR.MkExprStructureCreation k e r t rt)

    record <- HLIR.sanitizeRecord $ HLIR.MkTyRecord (HLIR.MkTyRowExtend k t.runIdentity rt.runIdentity)

    (fields, l1, bs1) <- unzip3 <$> forM (Map.toList creation) (\(field, (expr, ty)) -> do
        (expr', l1, bs1) <- convertExpression expr
        pure ((field, (expr', ty)), l1, bs1))

    (restE, l2, bs2) <- convertExpression rest

    recName <- freshSymbol

    -- let malloc = MLIR.MkExprApplication (MLIR.MkExprVariable "GC_MALLOC") [MLIR.MkExprSizeOf record]
    let def = MLIR.MkExprLet recName record Nothing
    let var = MLIR.MkExprVariable recName

    let wholeRecTy = getFields record

    let upds = [MLIR.MkExprUpdate (MLIR.MkExprStructureAccess var field) e' | (field, (e', _)) <- fields]

    let restTy = Map.withoutKeys wholeRecTy (Set.fromList $ map fst fields)

    let upds' = if isEmptyRowExpr rest
            then []
            else [ MLIR.MkExprUpdate
                    (MLIR.MkExprStructureAccess var field)
                    (MLIR.MkExprStructureAccess restE field)
                 | (field, _) <- Map.toList restTy
                 ]

    pure (var, concat l1 ++ l2 ++ [def] ++ upds' ++ upds, concat bs1 ++ bs2)
convertExpression HLIR.MkExprStructureEmpty = pure (MLIR.MkExprStructureCreation Map.empty, [], [])
convertExpression (HLIR.MkExprDereference e _) = do
    (e', l1, bs1) <- convertExpression e
    pure (MLIR.MkExprDereference e', l1, bs1)
convertExpression (HLIR.MkExprReference e ret) = do
    -- Creating a reference requires creating a let-binding for the inner expression,
    (e', l1, bs1) <- convertExpression e

    newVariable <- freshSymbol
    newRefVariable <- freshSymbol

    let ty = case ret.runIdentity of
            MLIR.MkTyPointer innerTy -> innerTy
            retTy -> retTy

    -- Creating the let bindings.
    -- It follows the following rules:
    --
    -- let <newVariable>: <ty> = <e'>;
    -- let <newRefVariable>: pointer<ty> = &<newVariable>;
    -- <newRefVariable>
    let ref = MLIR.MkExprReference (MLIR.MkExprVariable newVariable)
        letBinding = MLIR.MkExprLet newVariable ty (Just e')
        refBinding = MLIR.MkExprLet newRefVariable (MLIR.MkTyPointer ty) (Just ref)

    pure (MLIR.MkExprVariable newRefVariable, l1 ++ [letBinding, refBinding], bs1)
convertExpression (HLIR.MkExprUpdate (HLIR.MkExprApplication f args ty) value _) = do
    (app', l1, bs1) <- convertExpression (HLIR.MkExprReference (HLIR.MkExprApplication f args ty) (Identity (MLIR.MkTyPointer ty.runIdentity)))
    (value', l3, bs3) <- convertExpression value

    name <- freshSymbol

    let update_let = MLIR.MkExprLet name (MLIR.MkTyPointer ty.runIdentity) (Just app')
        update = MLIR.MkExprUpdate (MLIR.MkExprDereference (MLIR.MkExprVariable name)) value'

    pure (update, l1 ++ [update_let] ++ l3, bs1 ++ bs3)
convertExpression (HLIR.MkExprUpdate (HLIR.MkExprDereference e target) value _) = do
    (e', l1, bs1) <- convertExpression e
    (value', l2, bs2) <- convertExpression value

    newName <- freshSymbol

    -- We don't create early let-binding for *<e> because
    -- it would create a temporary variable that would
    -- disallow updates on pointers.
    let deref = MLIR.MkExprDereference (MLIR.MkExprVariable newName)
        update = MLIR.MkExprUpdate deref (MLIR.MkExprCast target.runIdentity value')
        letExpr = MLIR.MkExprLet newName (MLIR.MkTyPointer target.runIdentity) (Just e')

    pure (update, l1 ++ [letExpr] ++ l2, bs1 ++ bs2)
convertExpression (HLIR.MkExprSingleIf cond thenB branchTy) = do
    let conds = getConditions cond

    let isCF = isCFStatement thenB
    isVoid <- isVoidType branchTy.runIdentity

    if isCF || isVoid then do
        ifExpr <- local' $ createCondition conds thenB unitExpr (isCF, False) ""

        pure (MLIR.MkExprVariable "", ifExpr, [])
    else do
        newVariable <- freshSymbol
        let def = MLIR.MkExprLet newVariable branchTy.runIdentity Nothing
            varExpr = HLIR.MkExprVariable (HLIR.MkAnnotation newVariable branchTy) []

        ifExpr <- local' $ createCondition conds thenB varExpr (isCF, False) newVariable
        pure (MLIR.MkExprVariable newVariable, def : ifExpr, [])
convertExpression (HLIR.MkExprUpdate update value _) = do
    (update', l1, bs1) <- convertExpression update
    (value', l2, bs2) <- convertExpression value
    pure (MLIR.MkExprUpdate update' value', l1 ++ l2, bs1 ++ bs2)
convertExpression (HLIR.MkExprSizeOf t) = pure (MLIR.MkExprSizeOf t, [], [])
convertExpression (HLIR.MkExprLambda{}) =
    Err.compilerError
        "Lambdas should have been converted to top-level functions before ANF conversion."
convertExpression (HLIR.MkExprCast e t) = do
    (e', l1, bs1) <- convertExpression e
    pure (MLIR.MkExprCast t e', l1, bs1)
convertExpression (HLIR.MkExprWhile cond body _ inExpr) = do
    (cond', l1, bs1) <- convertExpression cond
    (body', l2, bs2) <- convertExpression body
    (inExpr', l3, bs3) <- convertExpression inExpr

    let bl = MLIR.MkExprBlock (l2 ++ [body'])
        whileExpr = MLIR.MkExprWhile cond' bl

    pure (inExpr', l1 ++ [whileExpr] ++ l3, bs1 ++ bs2 ++ bs3)
convertExpression (HLIR.MkExprIs e p ty) = do
    (e', l1, bs1) <- convertExpression e

    newName <- freshSymbol
    let nameBinding = MLIR.MkExprLet newName ty.runIdentity (Just e')
        nameExpr = MLIR.MkExprVariable newName

    (lets, conds) <- generateCondition nameExpr p

    let filteredLets = filter (\(_, ty', _) -> ty' /= MLIR.MkTyId "void") lets

    lets' <- mapM (\(name, ty', expr) -> do
            newVarName <- freshSymbol

            modifyIORef' variableRenamingMap (Map.insert name newVarName)

            pure $ MLIR.MkExprLet newVarName ty' (Just expr)
        ) filteredLets

    let cond =
            List.foldr
                (\a b -> MLIR.MkExprApplication (MLIR.MkExprVariable "&&") [a, b])
                (MLIR.MkExprLiteral (MLIR.MkLitBool True))
                conds

    pure (cond, l1, [nameBinding] <> lets' <> bs1)
convertExpression (HLIR.MkExprFunctionAccess{}) =
    Err.compilerError
        "Method calls should have been converted to function calls before ANF conversion."
convertExpression (HLIR.MkExprReturn e) = do
    (e', l1, bs1) <- convertExpression e
    pure (MLIR.MkExprReturn e', l1, bs1)
convertExpression HLIR.MkExprBreak = pure (MLIR.MkExprBreak, [], [])
convertExpression HLIR.MkExprContinue = pure (MLIR.MkExprContinue, [], [])
convertExpression (HLIR.MkExprLetPatternIn {}) =
    Err.compilerError
        "Let-pattern expressions should have been desugared before ANF conversion."

type Lets = [(Text, MLIR.Type, MLIR.Expression)]
type Conditions = [MLIR.Expression]

generateCondition ::
    (MonadIO m) =>
    MLIR.Expression ->
    HLIR.TLIR "pattern" ->
    m (Lets, Conditions)
generateCondition e (HLIR.MkPatternLocated _ node) = generateCondition e node
generateCondition e (HLIR.MkPatternLet ann) = do
    pure ([(ann.name, ann.typeValue.runIdentity, e)], [])
generateCondition e (HLIR.MkPatternVariable ann) = do
    let tag = MLIR.MkExprStructureAccess e "_tag"
        cond =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable "string_eq")
                [tag, MLIR.MkExprLiteral (MLIR.MkLitString ann.name)]

    pure ([], [cond])
generateCondition e (HLIR.MkPatternConstructor name patterns _) = do
    let tag = MLIR.MkExprStructureAccess e "_tag"
        cond =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable "string_eq")
                [tag, MLIR.MkExprLiteral (MLIR.MkLitString name)]

    (patternsLets, patternsConds) <-
        mapAndUnzipM
            ( \(p, idx) ->
                generateCondition
                    ( MLIR.MkExprStructureAccess
                        (MLIR.MkExprStructureAccess (MLIR.MkExprStructureAccess e "_data") name)
                        (name <> "_field" <> show idx)
                    )
                    p
            )
            (zip patterns [(1 :: Int) ..])

    pure (concat patternsLets, cond : concat patternsConds)
generateCondition _ HLIR.MkPatternWildcard = pure ([], [])
generateCondition x (HLIR.MkPatternLiteral l) = do
    let fun = case l of
            HLIR.MkLitInt{} -> "=="
            HLIR.MkLitChar{} -> "=="
            HLIR.MkLitString{} -> "str_equal"
            HLIR.MkLitBool{} -> "=="
            HLIR.MkLitFloat{} -> "=="

        cond =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable fun)
                [x, MLIR.MkExprLiteral l]
    pure ([], [cond])
generateCondition x (HLIR.MkPatternStructure fields) = do
    (fieldsLets, fieldsConds) <-
        mapAndUnzipM
            (\(name, p) -> generateCondition (MLIR.MkExprStructureAccess (MLIR.MkExprDereference x) name) p)
            (Map.toList fields)
    pure (concat fieldsLets, concat fieldsConds)

{-# NOINLINE variableRenamingMap #-}
variableRenamingMap :: IORef (Map Text Text)
variableRenamingMap = IO.unsafePerformIO (newIORef Map.empty)

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO (newIORef 0)

freshSymbol :: (MonadIO m) => m Text
freshSymbol = do
    modifyIORef' symbolCounter (+ 1)
    i <- readIORef symbolCounter
    pure $ "anf_tmp_" <> Text.pack (show i)

freshPrefixed :: (MonadIO m) => Text -> m Text
freshPrefixed prefix = do
    modifyIORef' symbolCounter (+ 1)
    i <- readIORef symbolCounter
    pure $ prefix <> "_" <> Text.pack (show i)

isEmptyRowExpr :: HLIR.TLIR "expression" -> Bool
isEmptyRowExpr HLIR.MkExprStructureEmpty = True
isEmptyRowExpr (HLIR.MkExprLocated _ e) = isEmptyRowExpr e
isEmptyRowExpr _ = False

getFields :: HLIR.Type -> Map Text HLIR.Type
getFields (HLIR.MkTyRecord t) = getFields t
getFields (HLIR.MkTyRowExtend field ty rest) = Map.insert field ty (getFields rest)
getFields HLIR.MkTyRowEmpty = Map.empty
getFields _ = error "Expected a record type"
