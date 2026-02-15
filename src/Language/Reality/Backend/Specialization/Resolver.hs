{-# LANGUAGE LambdaCase #-}

module Language.Reality.Backend.Specialization.Resolver where

import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Traversable qualified as Trav
import GHC.IO qualified as IO
import Language.Reality.Frontend.Typechecker.Monad qualified as M
import Language.Reality.Frontend.Typechecker.Unification qualified as M
import Language.Reality.Syntax.HLIR qualified as HLIR
import qualified Data.List as List

-- | SPECIALIZATION RESOLVER
-- | Resolve specializations in a program.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with specializations resolved.
-- | It replaces specialized function calls with their specialized versions.
-- | If a specialized version does not exist, it throws an error.
runSpecializationResolver ::
    (MonadIO m, M.MonadError M.Error m) =>
    [HLIR.TLIR "toplevel"] ->
    m [HLIR.TLIR "toplevel"]
runSpecializationResolver toplevels = do
    -- Resetting the specialization state before running the resolver.
    -- This ensures that each run starts with a clean state.
    writeIORef defaultSpecializer emptySpecializer

    allFunctionSignatures <- getAllFunctionSignatures toplevels

    modifyIORef' defaultSpecializer $ \s ->
        s{variables = allFunctionSignatures `Map.union` s.variables}

    resolved <- forM toplevels $ \n -> do
        (resolvedNode, newDefs) <- resolveSpecializationSingular n
        pure (newDefs ++ maybeToList resolvedNode)

    pure (concat resolved)

-- | Get all function signatures
getAllFunctionSignatures ::
    (MonadIO m) =>
    [HLIR.TLIR "toplevel"] ->
    m (Map Text (HLIR.Scheme HLIR.Type, Maybe (HLIR.TLIR "toplevel")))
getAllFunctionSignatures (HLIR.MkTopFunctionDeclaration ann params ret body : xs) = do
    rest <- getAllFunctionSignatures xs
    let paramTypes = map (.typeValue) params
        funcType = paramTypes HLIR.:->: ret
        scheme = HLIR.Forall ann.typeValue funcType
    pure $ Map.insert ann.name (scheme, Just (HLIR.MkTopFunctionDeclaration ann params ret body)) rest
getAllFunctionSignatures (HLIR.MkTopConstantDeclaration ann expr : xs) = do
    rest <- getAllFunctionSignatures xs
    let scheme = HLIR.Forall [] ann.typeValue
    pure $ Map.insert ann.name (scheme, Just (HLIR.MkTopConstantDeclaration ann expr)) rest
getAllFunctionSignatures (HLIR.MkTopLocated _ n : xs) = getAllFunctionSignatures (n : xs)
getAllFunctionSignatures (_ : xs) = getAllFunctionSignatures xs
getAllFunctionSignatures [] = pure Map.empty

-- | Resolve specialization in singular toplevel nodes.
-- | This function takes a toplevel node, and returns a toplevel node with
-- | specializations resolved.
resolveSpecializationSingular ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.TLIR "toplevel" ->
    m (Maybe (HLIR.TLIR "toplevel"), [HLIR.TLIR "toplevel"])
resolveSpecializationSingular (HLIR.MkTopLocated p n) = do
    HLIR.pushPosition p
    (resolved, newDefs) <- resolveSpecializationSingular n
    void HLIR.popPosition
    pure (HLIR.MkTopLocated p <$> resolved, newDefs)
resolveSpecializationSingular (HLIR.MkTopAnnotation exprs ret) = do
    (specExprs, newDefs, _) <-
        unzip3 <$> mapM
            resolveSpecializationInExpr
            exprs

    (specRet, newDefs2) <- resolveSpecializationSingular ret

    let allNewDefs = concat newDefs ++ newDefs2

    pure (HLIR.MkTopAnnotation specExprs <$> specRet, allNewDefs)
resolveSpecializationSingular (HLIR.MkTopExternLet ann) = do
    (specTy, newDefs) <- resolveSpecializationInType 0 ann.typeValue

    modifyIORef' defaultSpecializer $ \s ->
        s{rememberedNatives = Set.insert ann.name s.rememberedNatives}

    pure (Just $ HLIR.MkTopExternLet ann{HLIR.typeValue = specTy}, newDefs)
resolveSpecializationSingular (HLIR.MkTopConstantDeclaration ann expr) = do
    (typedExpr, newDefs, _) <- resolveSpecializationInExpr expr

    (specTy, newDefs2) <- resolveSpecializationInType 0 ann.typeValue

    let allNewDefs = newDefs ++ newDefs2

    let ann' = ann{HLIR.typeValue = specTy}

    modifyIORef' defaultSpecializer $ \s ->
        s{rememberedLocals = Set.insert ann.name s.rememberedLocals}

    pure (Just $ HLIR.MkTopConstantDeclaration ann' typedExpr, allNewDefs)
resolveSpecializationSingular (HLIR.MkTopLet binding value) = do
    (typedExpr, newDefs, _) <- resolveSpecializationInExpr value

    (specTy, newDefs2) <- resolveSpecializationInType 0 binding.typeValue.runIdentity

    let allNewDefs = newDefs ++ newDefs2

    let binding' = binding{HLIR.typeValue = Identity specTy}

    modifyIORef' defaultSpecializer $ \s ->
        s{rememberedLocals = Set.insert binding.name s.rememberedLocals}

    pure (Just $ HLIR.MkTopLet binding' typedExpr, allNewDefs)
resolveSpecializationSingular node@(HLIR.MkTopFunctionDeclaration ann params ret body)
    -- If no generics are found, we can directly resolve the function body
    -- and return the typed function declaration.
    | null ann.typeValue = do
        rememberedVars <-
            liftIO (readIORef defaultSpecializer) <&> rememberedVariables

        if Set.member ann.name rememberedVars
            then pure (Nothing, [])
            else do
                (specParams, newDefs) <-
                    mapAndUnzipM
                        ( \case
                            HLIR.MkAnnotation name ty -> do
                                (specTy, newDefs) <- resolveSpecializationInType 0 ty
                                pure (HLIR.MkAnnotation name specTy, newDefs)
                        )
                        params
                (specRet, newDefs2) <- resolveSpecializationInType 0 ret

                let arguments = Set.fromList (map (.name) specParams)

                modifyIORef' defaultSpecializer $ \s ->
                    s
                        { rememberedVariables = Set.insert ann.name s.rememberedVariables
                        , variables =
                            Map.insert
                                ann.name
                                (HLIR.Forall [] (map (.typeValue) params HLIR.:->: ret), Just node)
                                s.variables
                        }

                (typedBody, newDefs3, _) <- withLocals arguments $ resolveSpecializationInExpr body

                let allNewDefs = concat newDefs ++ newDefs2 ++ newDefs3

                pure
                    ( Just $ HLIR.MkTopFunctionDeclaration ann specParams specRet typedBody
                    , allNewDefs
                    )

    -- Otherwise, we register the function in the specialization state
    -- to be used later when resolving specialized calls.
    | otherwise = do
        let paramTypes = map (.typeValue) params
            funcType = paramTypes HLIR.:->: ret

        let scheme = HLIR.Forall ann.typeValue funcType

        modifyIORef' defaultSpecializer $ \s ->
            s{variables = Map.insert ann.name (scheme, Just node) s.variables}

        pure (Nothing, [])
resolveSpecializationSingular (HLIR.MkTopPublic n) = do
    (resolved, newDefs) <- resolveSpecializationSingular n
    pure (HLIR.MkTopPublic <$> resolved, newDefs)
resolveSpecializationSingular (HLIR.MkTopImplementation forType header parameters returnType body) = do
    let scheme =
            HLIR.Forall
                header.typeValue
                ((forType.typeValue : map (.typeValue) parameters) HLIR.:->: returnType)

    let newParams = forType : parameters

    modifyIORef' defaultSpecializer $ \s ->
        s
            { implementations =
                Map.insert
                    (header.name, scheme)
                    (Just $ HLIR.MkTopFunctionDeclaration header newParams returnType body)
                    s.implementations
            }

    pure (Nothing, [])
resolveSpecializationSingular (HLIR.MkTopProperty header parameters returnType) = do
    let scheme =
            HLIR.Forall header.typeValue (map (.typeValue) parameters HLIR.:->: returnType)

    modifyIORef' defaultSpecializer $ \s ->
        s{properties = Map.insert header.name scheme s.properties}

    pure (Nothing, [])
resolveSpecializationSingular (HLIR.MkTopExternalFunction ann params ret) = do
    remembered <-
        liftIO (readIORef defaultSpecializer) <&> rememberedNatives

    if Set.member ann.name remembered
        then
            pure (Nothing, [])
        else do
            modifyIORef' defaultSpecializer $ \s ->
                s{rememberedNatives = Set.insert ann.name s.rememberedNatives}
            (newParams, nss) <-
                unzip
                    <$> forM
                        params
                        ( \(HLIR.MkAnnotation name ty) -> do
                            (specTy, ns) <- resolveSpecializationInType 0 ty
                            pure (HLIR.MkAnnotation name specTy, ns)
                        )
            (specRet, ns) <- resolveSpecializationInType 0 ret

            let allNewDefs = concat nss ++ ns

            pure (Just $ HLIR.MkTopExternalFunction ann newParams specRet, allNewDefs)
resolveSpecializationSingular n@(HLIR.MkTopEnumeration ann constructors)
    | null ann.typeValue = do
        rememberedEnums <-
            liftIO (readIORef defaultSpecializer) <&> rememberedEnumerations

        let locals = Map.keys constructors

        if Set.member ann.name rememberedEnums
            then pure (Nothing, [])
            else do
                modifyIORef' defaultSpecializer $ \s ->
                    s{ rememberedEnumerations = Set.insert ann.name s.rememberedEnumerations
                     , rememberedLocals = Set.union (Set.fromList locals) s.rememberedLocals
                     , paramLessConstructors = Set.union
                         ( Set.fromList
                             [ name
                             | (name, Nothing) <- Map.toList constructors
                             ]
                         )
                         s.paramLessConstructors
                     }

                (ns, constructors') <- (sequence <$>) $ Trav.for constructors $ \case
                    Just tys -> do
                        (specTys, nss) <-
                            mapAndUnzipM (resolveSpecializationInType 0) tys
                        pure (concat nss, Just specTys)
                    Nothing -> pure ([], Nothing)


                pure (Just $ HLIR.MkTopEnumeration ann constructors', ns)
    | otherwise = do
        let header = HLIR.MkTyApp (HLIR.MkTyId ann.name) (map HLIR.MkTyQuantified ann.typeValue)

        let typedConstructors =
                Map.map
                    ( \case
                        Just tys -> tys HLIR.:->: header
                        Nothing -> header
                    )
                    constructors

        let typedSchemes = Map.map (\t -> (HLIR.Forall ann.typeValue t, Just n)) typedConstructors

        modifyIORef' defaultSpecializer $ \s ->
            s
                { enumerations =
                    Map.insert ann.name (HLIR.Forall ann.typeValue typedConstructors) s.enumerations
                , variables = typedSchemes `Map.union` s.variables
                }
        pure (Nothing, [])
resolveSpecializationSingular _ = pure (Nothing, [])

isFun :: HLIR.Type -> Bool
isFun (HLIR.MkTyFun _ _) = True
isFun _ = False

-- | Resolve specialization in expressions.
-- | This function takes an expression, and returns an expression with
-- | specializations resolved.
-- | It replaces specialized function calls with their specialized versions.
-- | If a specialized version does not exist, it throws an error.
resolveSpecializationInExpr ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.TLIR "expression" ->
    m (HLIR.TLIR "expression", [HLIR.TLIR "toplevel"], Set Text)
resolveSpecializationInExpr (HLIR.MkExprLocated p e) = do
    HLIR.pushPosition p
    (resolved, newDefs, bindings) <- resolveSpecializationInExpr e
    void HLIR.popPosition
    pure (HLIR.MkExprLocated p resolved, newDefs, bindings)
resolveSpecializationInExpr (HLIR.MkExprVariable ann _) = do
    (specAnn, defs, shouldWrap) <- resolveSpecializationForIdentifier ann

    paramLessConstructors' <- liftIO $ readIORef defaultSpecializer <&> paramLessConstructors
    let shouldWrap' =
            shouldWrap
                || Set.member ann.name paramLessConstructors'

    pure
        ( if shouldWrap' && not (isFun specAnn.typeValue.runIdentity)
            then
                HLIR.MkExprApplication (HLIR.MkExprVariable specAnn []) [] specAnn.typeValue
            else
                HLIR.MkExprVariable specAnn []
        , defs
        , mempty
        )
resolveSpecializationInExpr (HLIR.MkExprLiteral lit) = do
    pure (HLIR.MkExprLiteral lit, [], Set.empty)
resolveSpecializationInExpr (HLIR.MkExprLambda params ret body) = do
    (specParams, newDefs) <-
        mapAndUnzipM
            ( \case
                HLIR.MkAnnotation name ty -> do
                    (specTy, newDefs) <- resolveSpecializationInType 0 ty.runIdentity
                    pure (HLIR.MkAnnotation name (Identity specTy), newDefs)
            )
            params
    (specRet, newDefs2) <-
        resolveSpecializationInType 0 ret.runIdentity

    let arguments = Set.fromList (map (.name) specParams)

    (typedBody, newDefs3, bindings3) <- withLocals arguments $ resolveSpecializationInExpr body

    let allNewDefs = concat newDefs ++ newDefs2 ++ newDefs3

    pure (HLIR.MkExprLambda specParams (Identity specRet) typedBody, allNewDefs, bindings3)
resolveSpecializationInExpr (HLIR.MkExprLetIn binding value inExpr ret) = do
    (specBinding, newDefs) <- case binding of
        HLIR.MkAnnotation name ty -> do
            (specTy, newDefs) <- resolveSpecializationInType 0 ty.runIdentity
            pure (HLIR.MkAnnotation name (Identity specTy), newDefs)
    (typedValue, newDefs1, b1) <-
        withLocals (Set.singleton specBinding.name) $ resolveSpecializationInExpr value
    (typedInExpr, newDefs2, b2) <-
        withLocals (Set.singleton specBinding.name) $ resolveSpecializationInExpr inExpr
    (specRet, newDefs3) <-
        first Identity <$> resolveSpecializationInType 0 ret.runIdentity

    let allNewDefs = newDefs ++ newDefs1 ++ newDefs2 ++ newDefs3

    pure (HLIR.MkExprLetIn specBinding typedValue typedInExpr specRet, allNewDefs, b1 <> b2)
resolveSpecializationInExpr (HLIR.MkExprCondition cond thenB elseB thenType elseType) = do
    (typedCond, newDefs1, b1) <- resolveSpecializationInExpr cond
    (typedThen, newDefs2, _) <- withLocals b1 $ resolveSpecializationInExpr thenB
    (typedElse, newDefs3, _) <- resolveSpecializationInExpr elseB
    (specThenType, newDefs4) <-
        first Identity <$> resolveSpecializationInType 0 thenType.runIdentity
    (specElseType, newDefs5) <-
        first Identity <$> resolveSpecializationInType 0 elseType.runIdentity

    pure
        ( HLIR.MkExprCondition typedCond typedThen typedElse specThenType specElseType
        , newDefs1 ++ newDefs2 ++ newDefs3 ++ newDefs4 ++ newDefs5
        , mempty
        )
resolveSpecializationInExpr (HLIR.MkExprApplication callee args retTy) = do
    (typedCallee, newDefs1, b1) <- resolveSpecializationInExpr callee
    (typedArgs, newDefs2, b2) <- foldlM
        ( \(accArgs, accDefs, accB) arg -> do
            (typedArg, newDefs, b) <- withLocals accB $ resolveSpecializationInExpr arg
            pure (accArgs ++ [typedArg], accDefs ++ newDefs, accB <> b)
        )
        ([], [], mempty)
        args
    (specRetTy, newDefs3) <- resolveSpecializationInType 0 retTy.runIdentity

    pure
        ( HLIR.MkExprApplication typedCallee typedArgs (Identity specRetTy)
        , newDefs1 ++ newDefs2 ++ newDefs3
        , b1 <> b2
        )
resolveSpecializationInExpr (HLIR.MkExprStructureAccess struct field) = do
    (typedStruct, newDefs, b1) <- resolveSpecializationInExpr struct
    pure (HLIR.MkExprStructureAccess typedStruct field, newDefs, b1)
resolveSpecializationInExpr HLIR.MkExprStructureEmpty = pure (HLIR.MkExprStructureEmpty, [], mempty)
resolveSpecializationInExpr (HLIR.MkExprStructureCreation key value record ty rTy) = do
    (typedValue, newDefs1, b1) <- resolveSpecializationInExpr value
    (typedRecord, newDefs2, b2) <- resolveSpecializationInExpr record
    (specTy, newDefs3) <- resolveSpecializationInType 0 ty.runIdentity
    (specRTy, newDefs4) <- resolveSpecializationInType 0 rTy.runIdentity

    let allNewDefs = newDefs1 ++ newDefs2 ++ newDefs3 ++ newDefs4
        bs = [b1, b2]

    pure (HLIR.MkExprStructureCreation key typedValue typedRecord (Identity specTy) (Identity specRTy), allNewDefs, mconcat bs)
resolveSpecializationInExpr (HLIR.MkExprDereference e targetType) = do
    (typedE, newDefs, b1) <- resolveSpecializationInExpr e
    (specTargetType, newDefs2) <-
        first Identity <$> resolveSpecializationInType 0 targetType.runIdentity

    pure (HLIR.MkExprDereference typedE specTargetType, newDefs ++ newDefs2, b1)
resolveSpecializationInExpr (HLIR.MkExprReference e targetType) = do
    (typedE, newDefs, b1) <- resolveSpecializationInExpr e
    (specTargetType, newDefs2) <-
        first Identity <$> resolveSpecializationInType 0 targetType.runIdentity

    pure (HLIR.MkExprReference typedE specTargetType, newDefs ++ newDefs2, b1)
resolveSpecializationInExpr (HLIR.MkExprUpdate update value updateType) = do
    (typedUpdate, newDefs1, b1) <- resolveSpecializationInExpr update
    (typedValue, newDefs2, b2) <- resolveSpecializationInExpr value
    (typedUpdateType, newDefs3) <-
        first Identity <$> resolveSpecializationInType 0 updateType.runIdentity

    pure
        ( HLIR.MkExprUpdate typedUpdate typedValue typedUpdateType
        , newDefs1 ++ newDefs2 ++ newDefs3
        , b1 <> b2
        )
resolveSpecializationInExpr (HLIR.MkExprSizeOf t) = do
    (specTy, newDefs) <- resolveSpecializationInType 0 t
    pure (HLIR.MkExprSizeOf specTy, newDefs, mempty)
resolveSpecializationInExpr (HLIR.MkExprSingleIf cond thenB ret) = do
    (typedCond, newDefs1, b1) <- resolveSpecializationInExpr cond
    (typedThen, newDefs2, _) <- withLocals b1 $ resolveSpecializationInExpr thenB
    (specRet, newDefs3) <-
        first Identity <$> resolveSpecializationInType 0 ret.runIdentity

    pure
        ( HLIR.MkExprSingleIf typedCond typedThen specRet
        , newDefs1 ++ newDefs2 ++ newDefs3
        , mempty
        )
resolveSpecializationInExpr (HLIR.MkExprCast e t) = do
    (typedE, newDefs1, bs) <- resolveSpecializationInExpr e
    (specT, newDefs2) <- resolveSpecializationInType 0 t

    pure (HLIR.MkExprCast typedE specT, newDefs1 ++ newDefs2, bs)
resolveSpecializationInExpr (HLIR.MkExprWhile cond body ret inExpr) = do
    (typedCond, newDefs1, b1) <- resolveSpecializationInExpr cond
    (typedBody, newDefs2, _) <- withLocals b1 $ resolveSpecializationInExpr body
    (specRet, newDefs3) <-
        first Identity <$> resolveSpecializationInType 0 ret.runIdentity
    (typedInExpr, newDefs4, _) <- resolveSpecializationInExpr inExpr

    pure
        ( HLIR.MkExprWhile typedCond typedBody specRet typedInExpr
        , newDefs1 ++ newDefs2 ++ newDefs3 ++ newDefs4
        , mempty
        )
resolveSpecializationInExpr (HLIR.MkExprIs e p t) = do
    (typedE, newDefs1, b1) <- resolveSpecializationInExpr e
    (typedP, newDefs2, bindings) <- resolveSpecializationInPattern p
    (specT, newDefs3) <-
        first Identity <$> resolveSpecializationInType 0 t.runIdentity

    pure
        ( HLIR.MkExprIs typedE typedP specT
        , newDefs1 ++ newDefs2 ++ newDefs3
        , b1 <> bindings
        )
resolveSpecializationInExpr (HLIR.MkExprFunctionAccess{}) =
    M.compilerError
        "Function access should have been desugared before specialization resolution."
resolveSpecializationInExpr (HLIR.MkExprReturn e) = do
    (typedE, newDefs, b1) <- resolveSpecializationInExpr e
    pure (HLIR.MkExprReturn typedE, newDefs, b1)
resolveSpecializationInExpr HLIR.MkExprBreak = pure (HLIR.MkExprBreak, [], mempty)
resolveSpecializationInExpr HLIR.MkExprContinue = pure (HLIR.MkExprContinue, [], mempty)
resolveSpecializationInExpr (HLIR.MkExprLetPatternIn pat value inExpr ret) = do
    (typedPat, newDefs1, bindingsPat) <- resolveSpecializationInPattern pat
    (typedValue, newDefs2, _) <- withLocals bindingsPat $ resolveSpecializationInExpr value
    (typedInExpr, newDefs3, _) <- withLocals bindingsPat $ resolveSpecializationInExpr inExpr
    (specRet, newDefs4) <-
        first Identity <$> resolveSpecializationInType 0 ret.runIdentity

    pure
        ( HLIR.MkExprLetPatternIn typedPat typedValue typedInExpr specRet
        , newDefs1 ++ newDefs2 ++ newDefs3 ++ newDefs4
        , bindingsPat
        )

resolveSpecializationInPattern ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.TLIR "pattern" ->
    m (HLIR.TLIR "pattern", [HLIR.TLIR "toplevel"], Set Text)
resolveSpecializationInPattern (HLIR.MkPatternLocated p pat) = do
    HLIR.pushPosition p
    (resolved, newDefs, bindings) <- resolveSpecializationInPattern pat
    void HLIR.popPosition
    pure (HLIR.MkPatternLocated p resolved, newDefs, bindings)
resolveSpecializationInPattern (HLIR.MkPatternVariable ann) = do
    (specAnn, defs, _) <- resolveSpecializationForIdentifier ann
    pure (HLIR.MkPatternVariable specAnn, defs, mempty)
resolveSpecializationInPattern (HLIR.MkPatternLiteral lit) = do
    pure (HLIR.MkPatternLiteral lit, [], mempty)
resolveSpecializationInPattern (HLIR.MkPatternStructure fields) = do
    (typedFields, newDefs2, bindings) <-
        unzip3
            <$> mapM
                ( \(name, pat) -> do
                    (typedPat, defs, bindings) <- resolveSpecializationInPattern pat
                    pure ((name, typedPat), defs, bindings)
                )
                (Map.toList fields)
    let fieldMap = Map.fromList typedFields
        allNewDefs = concat newDefs2

    pure (HLIR.MkPatternStructure fieldMap, allNewDefs, mconcat bindings)
resolveSpecializationInPattern HLIR.MkPatternWildcard = do
    pure (HLIR.MkPatternWildcard, [], mempty)
resolveSpecializationInPattern (HLIR.MkPatternConstructor name patterns ty) = do
    (specName, defs, _) <-
        resolveSpecializationForIdentifier (HLIR.MkAnnotation name ty)
    (typedPatterns, newDefs, bindings) <-
        unzip3 <$> mapM resolveSpecializationInPattern patterns

    pure
        ( HLIR.MkPatternConstructor specName.name typedPatterns specName.typeValue
        , defs <> concat newDefs
        , mconcat bindings
        )
resolveSpecializationInPattern (HLIR.MkPatternLet binding) = do
    (specBinding, newDefs) <- case binding of
        HLIR.MkAnnotation name ty -> do
            (specTy, newDefs) <- resolveSpecializationInType 0 ty.runIdentity
            pure (HLIR.MkAnnotation name (Identity specTy), newDefs)
    pure (HLIR.MkPatternLet specBinding, newDefs, Set.singleton binding.name)

-- | Apply a substitution to all types in an expression.
-- | This function takes a substitution map and an expression, and returns
-- | an expression with the substitution applied to all types.
-- | The expected behavior is that all types in the expression are replaced
-- | according to the substitution map.
applySubstInExpr ::
    (MonadIO m) =>
    Map Text HLIR.Type -> HLIR.TLIR "expression" -> m (HLIR.TLIR "expression")
applySubstInExpr subst (HLIR.MkExprLocated p e) =
    HLIR.MkExprLocated p <$> applySubstInExpr subst e
applySubstInExpr subst (HLIR.MkExprVariable ann@(HLIR.MkAnnotation _ ty) types) = do
    newType <- M.applySubstitution subst ty.runIdentity
    pure (HLIR.MkExprVariable (ann{HLIR.typeValue = Identity newType}) types)
applySubstInExpr _ (HLIR.MkExprLiteral lit) = pure (HLIR.MkExprLiteral lit)
applySubstInExpr subst (HLIR.MkExprLambda params ret body) = do
    newParams <- forM params $ \(HLIR.MkAnnotation name ty) -> do
        newTy <- M.applySubstitution subst ty.runIdentity
        pure (HLIR.MkAnnotation name (Identity newTy))
    newRet <- M.applySubstitution subst ret.runIdentity
    newBody <- applySubstInExpr subst body
    pure (HLIR.MkExprLambda newParams (Identity newRet) newBody)
applySubstInExpr subst (HLIR.MkExprLetIn binding value inExpr ret) =
    do
        newBinding <- case binding of
            HLIR.MkAnnotation name ty -> do
                newTy <- M.applySubstitution subst ty.runIdentity
                pure (HLIR.MkAnnotation name (Identity newTy))
        newValue <- applySubstInExpr subst value
        newInExpr <- applySubstInExpr subst inExpr
        newRet <- M.applySubstitution subst ret.runIdentity

        pure (HLIR.MkExprLetIn newBinding newValue newInExpr (Identity newRet))
applySubstInExpr subst (HLIR.MkExprCondition cond thenB elseB thenType elseType) =
    do
        newCond <- applySubstInExpr subst cond
        newThenB <- applySubstInExpr subst thenB
        newElseB <- applySubstInExpr subst elseB

        newThenType <- M.applySubstitution subst thenType.runIdentity
        newElseType <- M.applySubstitution subst elseType.runIdentity

        pure (HLIR.MkExprCondition newCond newThenB newElseB (Identity newThenType) (Identity newElseType))
applySubstInExpr subst (HLIR.MkExprApplication callee args retTy) = do
    newCallee <- applySubstInExpr subst callee
    newArgs <- mapM (applySubstInExpr subst) args
    newRetTy <- M.applySubstitution subst retTy.runIdentity
    pure (HLIR.MkExprApplication newCallee newArgs (Identity newRetTy))
applySubstInExpr subst (HLIR.MkExprStructureAccess struct field) = do
    newStruct <- applySubstInExpr subst struct
    pure (HLIR.MkExprStructureAccess newStruct field)
applySubstInExpr subst (HLIR.MkExprStructureCreation key value record ty rTy) = do
    newValue <- applySubstInExpr subst value
    newRecord <- applySubstInExpr subst record
    newTy <- M.applySubstitution subst ty.runIdentity
    newRTy <- M.applySubstitution subst rTy.runIdentity

    pure (HLIR.MkExprStructureCreation key newValue newRecord (Identity newTy) (Identity newRTy))
applySubstInExpr subst (HLIR.MkExprDereference e targetType) = do
    newE <- applySubstInExpr subst e
    newTargetType <- M.applySubstitution subst targetType.runIdentity

    pure (HLIR.MkExprDereference newE (Identity newTargetType))
applySubstInExpr subst (HLIR.MkExprReference e targetType) = do
    newE <- applySubstInExpr subst e
    newTargetType <- M.applySubstitution subst targetType.runIdentity

    pure (HLIR.MkExprReference newE (Identity newTargetType))
applySubstInExpr subst (HLIR.MkExprUpdate update value updateType) = do
    newUpdate <- applySubstInExpr subst update
    newValue <- applySubstInExpr subst value
    newType <- M.applySubstitution subst updateType.runIdentity

    pure (HLIR.MkExprUpdate newUpdate newValue (Identity newType))
applySubstInExpr subst (HLIR.MkExprSizeOf t) = do
    newT <- M.applySubstitution subst t
    pure (HLIR.MkExprSizeOf newT)
applySubstInExpr subst (HLIR.MkExprSingleIf cond thenB ret) = do
    newCond <- applySubstInExpr subst cond
    newThenB <- applySubstInExpr subst thenB
    newRet <- M.applySubstitution subst ret.runIdentity

    pure (HLIR.MkExprSingleIf newCond newThenB (Identity newRet))
applySubstInExpr subst (HLIR.MkExprCast e t) = do
    newE <- applySubstInExpr subst e
    newT <- M.applySubstitution subst t
    pure (HLIR.MkExprCast newE newT)
applySubstInExpr subst (HLIR.MkExprWhile cond body ret inExpr) = do
    newCond <- applySubstInExpr subst cond
    newBody <- applySubstInExpr subst body
    newRet <- M.applySubstitution subst ret.runIdentity
    newInExpr <- applySubstInExpr subst inExpr
    pure (HLIR.MkExprWhile newCond newBody (Identity newRet) newInExpr)
applySubstInExpr subst (HLIR.MkExprIs e p t) = do
    newE <- applySubstInExpr subst e
    newP <- applySubstInPattern subst p
    newT <- M.applySubstitution subst t.runIdentity
    pure (HLIR.MkExprIs newE newP (Identity newT))
applySubstInExpr subst (HLIR.MkExprFunctionAccess f this ty args) = do
    newThis <- applySubstInExpr subst this
    newArgs <- mapM (applySubstInExpr subst) args
    pure (HLIR.MkExprFunctionAccess f newThis ty newArgs)
applySubstInExpr subst (HLIR.MkExprReturn e) = do
    newE <- applySubstInExpr subst e
    pure (HLIR.MkExprReturn newE)
applySubstInExpr _ HLIR.MkExprBreak = pure HLIR.MkExprBreak
applySubstInExpr _ HLIR.MkExprContinue = pure HLIR.MkExprContinue
applySubstInExpr subst (HLIR.MkExprLetPatternIn pat value inExpr ret) = do
    newPat <- applySubstInPattern subst pat
    newValue <- applySubstInExpr subst value
    newInExpr <- applySubstInExpr subst inExpr
    newRet <- M.applySubstitution subst ret.runIdentity

    pure (HLIR.MkExprLetPatternIn newPat newValue newInExpr (Identity newRet))
applySubstInExpr _ HLIR.MkExprStructureEmpty = pure HLIR.MkExprStructureEmpty

applySubstInPattern ::
    (MonadIO m) =>
    Map Text HLIR.Type ->
    HLIR.TLIR "pattern" ->
    m (HLIR.TLIR "pattern")
applySubstInPattern subst (HLIR.MkPatternLocated p pat) = do
    HLIR.pushPosition p
    newPat <- applySubstInPattern subst pat
    void HLIR.popPosition
    pure (HLIR.MkPatternLocated p newPat)
applySubstInPattern _ HLIR.MkPatternWildcard = do
    pure HLIR.MkPatternWildcard
applySubstInPattern subst (HLIR.MkPatternVariable ann) = do
    newTy <- M.applySubstitution subst ann.typeValue.runIdentity
    pure (HLIR.MkPatternVariable ann{HLIR.typeValue = Identity newTy})
applySubstInPattern _ (HLIR.MkPatternLiteral lit) = do
    pure (HLIR.MkPatternLiteral lit)
applySubstInPattern subst (HLIR.MkPatternStructure fields) = do
    newFields <- mapM (applySubstInPattern subst) fields
    pure (HLIR.MkPatternStructure newFields)
applySubstInPattern subst (HLIR.MkPatternLet binding) = do
    newBinding <- case binding of
        HLIR.MkAnnotation name ty -> do
            newTy <- M.applySubstitution subst ty.runIdentity
            pure (HLIR.MkAnnotation name (Identity newTy))
    pure (HLIR.MkPatternLet newBinding)
applySubstInPattern subst (HLIR.MkPatternConstructor name patterns ty) = do
    newPatterns <- mapM (applySubstInPattern subst) patterns
    newTy <- M.applySubstitution subst ty.runIdentity

    pure (HLIR.MkPatternConstructor name newPatterns (Identity newTy))

type ShouldWrap = Bool

-- | Resolve specialization for identifiers.
-- | This function takes an identifier and a associated type
-- | (which may be specialized), and returns an expression with the
-- | specialization resolved.
resolveSpecializationForIdentifier ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.Annotation (Identity HLIR.Type) ->
    m (HLIR.Annotation (Identity HLIR.Type), [HLIR.TLIR "toplevel"], ShouldWrap)
resolveSpecializationForIdentifier (HLIR.MkAnnotation name (Identity ty)) = do
    specState <- liftIO $ readIORef defaultSpecializer
    -- Fetching the variable scheme if it exists, and also the toplevel
    -- declaration associated with it.
    case Map.lookup name (variables specState) of
        Just (scheme@(HLIR.Forall qvars _), Just toplevel) -> do
            -- Instantiating the scheme to get a concrete type and a substitution
            -- that maps the quantified variables to concrete types.
            (schemeType, subst) <- M.instantiateAndSub scheme

            void $ ty `M.isSubtypeOf` schemeType

            -- Checking if the toplevel is a function declaration.
            -- If it is not, there must be an error somewhere else, because
            -- only functions can be specialized as they're the only nodes
            -- to capture generics.
            case toplevel of
                HLIR.MkTopFunctionDeclaration
                    { parameters
                    , returnType
                    , body
                    , name = _
                    } -> do
                        -- Re-ordering the resolved types according to the scheme quantified
                        -- variables to create a consistent name
                        -- (e.g., f_a_b and f_b_a should not refer to the same specialization)
                        -- This is important to avoid duplicating work and creating
                        -- wrong specializations.

                        tyAsText <- HLIR.prettify ty
                        let orderedVars = flip map qvars $ \var -> Map.findWithDefault (HLIR.MkTyQuantified var) var subst
                            newName
                                | null orderedVars = name
                                | otherwise = name <> "_" <> tyAsText

                        -- Checking if we already created this specialization
                        -- to avoid duplicating work.
                        if Set.member newName specState.rememberedVariables
                            then do
                                (newTy, ns) <- resolveSpecializationInType 0 =<< M.applySubstitution subst ty
                                let newAnn = HLIR.MkAnnotation newName (Identity newTy)
                                pure (newAnn, ns, False)
                            else do
                                -- Marking this specialization as created
                                modifyIORef' defaultSpecializer $ \s ->
                                    s{rememberedVariables = Set.insert newName s.rememberedVariables}

                                -- Applying the substitution to the parameters, return type, and body
                                -- of the function.
                                -- This creates the specialized version of the function.
                                -- Note that we also need to recursively resolve specializations
                                -- in the body, as it may contain calls to other specialized functions.
                                -- This ensures that the entire function is correctly specialized.
                                (specParameters, nss) <-
                                    unzip
                                        <$> Trav.for
                                            parameters
                                            ( \(HLIR.MkAnnotation paramName paramType) -> do
                                                (newParamType, ns) <-
                                                    resolveSpecializationInType 0 =<< M.applySubstitution subst paramType
                                                pure (HLIR.MkAnnotation paramName newParamType, ns)
                                            )

                                (specReturnType, ns) <-
                                    resolveSpecializationInType 0 =<< M.applySubstitution subst returnType

                                let arguments = Set.fromList (map (.name) specParameters)

                                newBody <- applySubstInExpr subst body
                                (specBody, newDefs, _) <-
                                    withLocals arguments $ resolveSpecializationInExpr newBody

                                -- Creating the new specialized function declaration
                                -- with the new name and specialized types.
                                let newFunction =
                                        HLIR.MkTopFunctionDeclaration
                                            { HLIR.name = HLIR.MkAnnotation newName []
                                            , HLIR.parameters = specParameters
                                            , HLIR.returnType = specReturnType
                                            , HLIR.body = specBody
                                            }

                                -- Combining all new definitions obtained during the resolution
                                -- including the new specialized function.
                                let allNewDefs = concat nss ++ ns ++ newDefs ++ [newFunction]

                                -- Building the new annotation type for the specialized function.
                                -- This is important for further type checking and resolution.
                                let funcType = map (.typeValue) specParameters HLIR.:->: specReturnType

                                pure (HLIR.MkAnnotation newName (Identity funcType), allNewDefs, False)
                HLIR.MkTopEnumeration header constructors -> do
                    -- Re-ordering the resolved types according to the scheme quantified
                    -- variables to create a consistent name
                    -- (e.g., f_a_b and f_b_a should not refer to the same specialization)
                    -- This is important to avoid duplicating work and creating
                    -- wrong specializations.
                    let orderedVars = flip map qvars $ \var -> Map.findWithDefault (HLIR.MkTyQuantified var) var subst

                    newEnumName <- HLIR.prettify (HLIR.MkTyApp (HLIR.MkTyId header.name) orderedVars)

                    let newVarName = name <> "@" <> Text.intercalate "_" (map toText orderedVars)

                    -- Checking if we already created this specialization
                    -- to avoid duplicating work.
                    if Set.member newEnumName specState.rememberedEnumerations
                        then do
                            (newTy, ns) <- resolveSpecializationInType 0 =<< M.applySubstitution subst ty

                            let newAnn = HLIR.MkAnnotation newVarName (Identity newTy)
                            pure (newAnn, ns, True)
                        else do
                            -- Marking this specialization as created
                            modifyIORef' defaultSpecializer $ \s ->
                                s{rememberedEnumerations = Set.insert newEnumName s.rememberedEnumerations}

                            let header' = HLIR.MkAnnotation newEnumName []

                            (nss, newFields) <-
                                sequence
                                    <$> forM
                                        (Map.toList constructors)
                                        ( \mTys -> do
                                            case mTys of
                                                (n, Just tys) -> do
                                                    (newTys, nss) <-
                                                        mapAndUnzipM
                                                            ( \ty' -> do
                                                                sTy <- M.applySubstitution subst ty'
                                                                resolveSpecializationInType 0 sTy
                                                            )
                                                            tys
                                                    pure
                                                        (nss, (n <> "@" <> Text.intercalate "_" (map toText orderedVars), Just newTys))
                                                (n, Nothing) ->
                                                    pure ([], (n <> "@" <> Text.intercalate "_" (map toText orderedVars), Nothing))
                                        )

                            let newEnum = HLIR.MkTopEnumeration header' (Map.fromList newFields)

                            let ns = concat nss <> [newEnum]

                            pure (HLIR.MkAnnotation newVarName (Identity schemeType), ns, True)
                _ -> M.throw (M.VariableNotFound name)
        _ -> resolveSpecializationForImplementation name ty

resolveSpecializationForImplementation ::
    (MonadIO m, M.MonadError M.Error m) =>
    Text ->
    HLIR.Type ->
    m (HLIR.Annotation (Identity HLIR.Type), [HLIR.TLIR "toplevel"], ShouldWrap)
resolveSpecializationForImplementation name ty = do
    specState <- liftIO $ readIORef defaultSpecializer

    -- Look for an implementation that matches the name and type
    -- Algorithm:
    -- 1. For each implementation with the same name
    -- 2. Instantiate its scheme to get a concrete type and a substitution
    -- 3. Check if the concrete type is a subtype of the given type
    -- 4. If it is, return the implementation, substitution, and scheme
    -- 5. If none match, return Nothing
    result1 <-
        findImplementationMatching (Map.toList specState.implementations) name ty

    -- Also look for a property with the same name
    -- This is used to create the correct name according to the
    -- property type variables.
    let result2 = Map.lookup name specState.properties

    -- Combining both results to get the final result
    let result = case (result1, result2) of
            (Just (node, subst, _), Just propScheme) ->
                Just (node, subst, propScheme)
            (Just (node, subst, scheme), Nothing) ->
                Just (node, subst, scheme)
            _ -> Nothing

    case result of
        Just (node, implSubst, propScheme@(HLIR.Forall _ _)) -> do
            -- Creating the final substitution by instantiating the property scheme
            -- and ensuring that the implementation type is a subtype of the property type
            (propTy, _) <- M.instantiateAndSub propScheme
            void $ ty `M.isSubtypeOf` propTy

            tyAsText <- HLIR.prettify ty

            -- Re-ordering the resolved types according to the property scheme quantified
            -- variables to create a consistent name
            let newName = name <> "_" <> tyAsText
            -- Checking if the node is a function declaration, otherwise there must
            -- be an error somewhere else. We just handle it gracefully here.
            case node of
                Just HLIR.MkTopFunctionDeclaration
                    { parameters
                    , returnType
                    , body
                    , name = _
                    } -> do
                        -- Checking if we already created this specialization
                        -- to avoid duplicating work.
                        if Set.member newName specState.rememberedImplementations
                            then do
                                (newTy, ns) <- resolveSpecializationInType 0 =<< M.applySubstitution implSubst ty
                                pure (HLIR.MkAnnotation newName (Identity newTy), ns, False)
                            else do
                                -- Marking this specialization as created
                                -- to avoid duplicating work in the future.
                                modifyIORef' defaultSpecializer $ \s ->
                                    s{rememberedImplementations = Set.insert newName s.rememberedImplementations}

                                -- Applying the implementation substitution to the parameters,
                                -- return type, and body of the function.
                                (specParameters, nss) <-
                                    unzip
                                        <$> Trav.for
                                            parameters
                                            ( \(HLIR.MkAnnotation paramName paramType) -> do
                                                (newParamType, ns) <-
                                                    resolveSpecializationInType 0 =<< M.applySubstitution implSubst paramType
                                                pure (HLIR.MkAnnotation paramName newParamType, ns)
                                            )
                                (specReturnType, ns) <-
                                    resolveSpecializationInType 0 =<< M.applySubstitution implSubst returnType

                                newBody <- applySubstInExpr implSubst body

                                let arguments = Set.fromList $ map (.name) specParameters

                                -- Recursively resolving specializations in the body
                                -- in case there are nested specializations.
                                -- This also ensures that any new toplevel definitions
                                -- created during the resolution are collected.
                                -- This is important to maintain the integrity of the program.
                                (specBody, newDefs, _) <-
                                    withLocals arguments $ resolveSpecializationInExpr newBody

                                -- Creating the new specialized function declaration
                                -- with the new name and specialized types.
                                -- Note that implementations become regular functions,
                                -- ensuring better performance and inlining opportunities.
                                let newImpl =
                                        HLIR.MkTopFunctionDeclaration
                                            { HLIR.name = HLIR.MkAnnotation newName []
                                            , HLIR.parameters = specParameters
                                            , HLIR.returnType = specReturnType
                                            , HLIR.body = specBody
                                            }

                                -- Combining all new definitions obtained during the resolution
                                -- including the new specialized function.
                                let allNewDefs = concat nss ++ ns ++ newDefs ++ [newImpl]

                                -- Building the new annotation type for the specialized function.
                                -- This is important for further type checking and resolution.
                                let funcType = map (.typeValue) specParameters HLIR.:->: specReturnType

                                pure (HLIR.MkAnnotation newName (Identity funcType), allNewDefs, False)
                Nothing -> do
                    (newTy, ns) <- resolveSpecializationInType 0 =<< M.applySubstitution implSubst ty
                    pure (HLIR.MkAnnotation newName (Identity newTy), ns, False)
                _ -> M.throw (M.ImplementationNotFound name ty)
        Nothing -> do
            -- If no implementation or property was found, default to
            -- the original name and type.

            (newTy, ns) <- resolveSpecializationInType 0 ty

            if name `Set.member` (specState.rememberedNatives <> specState.rememberedLocals)
                then pure (HLIR.MkAnnotation name (Identity newTy), ns, False)
                else do
                    M.throw (M.ImplementationNotFound name ty)
  where
    -- The actual implementation matching algorithm
    -- as described above.
    -- It recursively checks each implementation
    -- until it finds a match or exhausts the list.
    -- If a match is found, it returns the implementation,
    -- the substitution, and the scheme.
    -- If none match, it returns Nothing.
    --
    -- This function is crucial for resolving
    -- specialized function calls correctly.
    -- It ensures that the most appropriate implementation
    -- is selected based on the provided type.
    findImplementationMatching ::
        (MonadIO m, M.MonadError M.Error m) =>
        [((Text, HLIR.Scheme HLIR.Type), Maybe (HLIR.TLIR "toplevel"))] ->
        Text ->
        HLIR.Type ->
        m (Maybe (Maybe (HLIR.TLIR "toplevel"), M.Substitution, HLIR.Scheme HLIR.Type))
    findImplementationMatching [] _ _ = pure Nothing
    findImplementationMatching (((implName, implScheme), toplevel) : xs) varName ty'
        | implName == varName = do
            -- Instantiating the implementation scheme to get a concrete type
            -- and a substitution map.
            -- Then performing alias removal to simplify the type.
            (implTy, sub) <- M.instantiateAndSub implScheme
            aliasedImplTy <- M.performAliasRemoval implTy

            -- Checking if the aliased implementation type is a subtype of the given type
            -- without performing any unification.
            -- This is done to ensure that the implementation can be used
            -- for the specialized function call.
            result <- runExceptT $ M.applySubtypeRelation False ty' aliasedImplTy

            case result of
                Right _ -> do
                    -- If it is a subtype, we have found a matching implementation.
                    -- We can fully apply subtype relation to get the final substitution.
                    void $ ty' `M.isSubtypeOf` aliasedImplTy
                    pure $ Just (toplevel, sub, implScheme)
                Left _ -> findImplementationMatching xs name ty'
        | otherwise = findImplementationMatching xs name ty'

-- | Resolve specialization in types.
-- | This function takes a type, and returns a type with specializations resolved.
-- | It replaces specialized type applications with their specialized versions.
-- | If a specialized version does not exist, it throws an error.
resolveSpecializationInType ::
    (MonadIO m, M.MonadError M.Error m) =>
    Integer ->
    HLIR.Type ->
    m (HLIR.Type, [HLIR.TLIR "toplevel"])
resolveSpecializationInType depth ty
    | depth > 20 = M.throw (M.RecursionLimitExceeded 20)
    | otherwise = resolveSpecializationInType' depth =<< HLIR.simplify ty

resolveSpecializationInType' ::
    (MonadIO m, M.MonadError M.Error m) =>
    Integer ->
    HLIR.Type ->
    m (HLIR.Type, [HLIR.TLIR "toplevel"])
resolveSpecializationInType' n (HLIR.MkTyPointer ty) = do
    (typedTy, newDefs) <- resolveSpecializationInType (n + 1) ty
    pure (HLIR.MkTyPointer typedTy, newDefs)
resolveSpecializationInType' n (args HLIR.:->: ret) = do
    (typedArgs, newDefs1) <- mapAndUnzipM (resolveSpecializationInType (n + 1)) args
    (typedRet, newDefs2) <- resolveSpecializationInType (n + 1) ret

    pure (typedArgs HLIR.:->: typedRet, concat newDefs1 ++ newDefs2)
resolveSpecializationInType' n (HLIR.MkTyApp (HLIR.MkTyId base) args) = do
    maybeResolveStructure n base args
resolveSpecializationInType' n (HLIR.MkTyId name) =
    maybeResolveStructure n name []
resolveSpecializationInType' n (HLIR.MkTyApp base args) = do
    (typedBase, newDefs1) <- resolveSpecializationInType (n + 1) base
    (typedArgs, newDefs2) <- mapAndUnzipM (resolveSpecializationInType (n + 1)) args

    pure (HLIR.MkTyApp typedBase typedArgs, newDefs1 ++ concat newDefs2)
resolveSpecializationInType' n (HLIR.MkTyVar ref) = do
    ty <- liftIO $ readIORef ref
    case ty of
        HLIR.Link ty' -> resolveSpecializationInType n ty'
        HLIR.Unbound{} -> pure (HLIR.MkTyVar ref, [])
resolveSpecializationInType' _ (HLIR.MkTyQuantified name) = do
    pure (HLIR.MkTyQuantified name, [])
resolveSpecializationInType' n (HLIR.MkTyRecord ty) = do
    let (fields, row) = getAllFields ty
        sortedRecord = Map.foldrWithKey HLIR.MkTyRowExtend row fields

    (typedTy, newDefs) <- resolveSpecializationInType (n + 1) sortedRecord
    pure (HLIR.MkTyRecord typedTy, newDefs)
resolveSpecializationInType' _ HLIR.MkTyRowEmpty = pure (HLIR.MkTyRowEmpty, [])
resolveSpecializationInType' n (HLIR.MkTyRowExtend label ty rest) = do
    (typedTy, newDefs1) <- resolveSpecializationInType (n + 1) ty
    (typedRest, newDefs2) <- resolveSpecializationInType (n + 1) rest
    pure (HLIR.MkTyRowExtend label typedTy typedRest, newDefs1 ++ newDefs2)

getAllFields :: HLIR.Type -> (Map Text HLIR.Type, HLIR.Type)
getAllFields (HLIR.MkTyRecord fields) = getAllFields fields
getAllFields (HLIR.MkTyRowExtend label ty rest) =
    let (restFields, restRow) = getAllFields rest
     in (Map.insert label ty restFields, restRow)
getAllFields HLIR.MkTyRowEmpty = (Map.empty, HLIR.MkTyRowEmpty)
getAllFields other = (Map.empty, other)

-- | Attempt to resolve a structure specialization.
-- | This function takes a structure name and a list of type arguments,
-- | and returns a specialized type along with any new toplevel definitions
-- | that were created during the specialization process.
maybeResolveStructure ::
    (MonadIO m, M.MonadError M.Error m) =>
    Integer ->
    Text ->
    [HLIR.Type] ->
    m (HLIR.Type, [HLIR.TLIR "toplevel"])
maybeResolveStructure depth name args = do
    -- Reading the current state of the specializer
    specState <- liftIO $ readIORef defaultSpecializer

    -- Looking up the structure in the specializer state
    -- If found, we proceed to specialize it
    -- If not found, we assume it's a regular type and return it as is
    case Map.lookup name specState.structures of
        Just scheme@(HLIR.Forall qvars instMap) -> do
            -- Instantiating the scheme to get a concrete type and a substitution
            -- that maps the quantified variables to concrete types.
            (_, sub) <- M.instantiateMapWithSub scheme

            -- Ensuring the number of type arguments matches the number of
            -- type variables in the structure definition
            when (Map.size sub /= length args)
                $ M.throw (M.InvalidArgumentQuantity (Map.size instMap) (length args))

            -- Creating a substitution map that maps the structure's type variables
            -- to the provided type arguments
            let s = Map.fromList (zip (Map.keys sub) args) <> sub

            -- Creating a unique name for the specialized structure
            -- This is done by appending the type arguments to the original name
            -- (e.g., MyStruct_Int_Bool for MyStruct<Int, Bool>)
            let orderedVars = flip map qvars $ \var -> Map.findWithDefault (HLIR.MkTyQuantified var) var s
            specName <- HLIR.prettify (HLIR.MkTyApp (HLIR.MkTyId name) orderedVars)

            -- Checking if we have already created this specialization
            -- to avoid duplicating work
            -- If it exists, we simply return the specialized type
            -- If it does not exist, we create a new structure declaration
            -- and update the specializer state accordingly
            if Set.member specName specState.rememberedStructures
                then pure (HLIR.MkTyId specName, [])
                else do
                    modifyIORef' defaultSpecializer $ \s' ->
                        s'
                            { rememberedStructures = Set.insert specName s'.rememberedStructures
                            , structures = Map.insert specName (HLIR.Forall [] Map.empty) s'.structures
                            }

                    -- Applying the substitution to each field type in the structure
                    -- and recursively resolving specializations in those types
                    -- This ensures that nested specializations are also handled correctly
                    -- The result is a list of specialized fields and any new toplevel definitions
                    -- that were created during the specialization process
                    (ns, specializedFields) <-
                        sequence
                            <$> Map.traverseWithKey
                                (\_ ty -> swap <$> (resolveSpecializationInType (depth + 1) =<< M.applySubstitution s ty))
                                instMap

                    when (name == "Annotation") $ do
                        liftIO $ putStrLn $ "Specialized Annotation to " ++ show specName ++ " with fields: " ++ show specializedFields

                    let structMembers = Map.toList specializedFields
                        structMembers' = List.foldl (\acc (n, ty) ->
                                HLIR.MkTyRowExtend n ty acc) HLIR.MkTyRowEmpty structMembers

                    -- Creating the new specialized structure declaration
                    -- with the new name and specialized fields
                    -- This declaration is added to the list of new toplevel definitions
                    -- that will be returned
                    let newStruct =
                            HLIR.MkTopTypeAlias
                                { HLIR.name = HLIR.MkAnnotation specName []
                                , HLIR.boundType = structMembers'
                                }

                    -- Updating the specializer state to include the new structure
                    -- and marking it as remembered to avoid future duplication
                    -- This is crucial for maintaining the integrity of the specialization process
                    -- and ensuring that subsequent requests for the same specialization
                    -- are handled efficiently
                    modifyIORef' defaultSpecializer $ \s' ->
                        s'
                            { structures =
                                Map.insert specName (HLIR.Forall [] specializedFields) s'.structures
                            }

                    pure (HLIR.MkTyId specName, ns ++ [newStruct])
        Nothing -> maybeResolveEnumeration depth name args

maybeResolveEnumeration ::
    (MonadIO m, M.MonadError M.Error m) =>
    Integer ->
    Text ->
    [HLIR.Type] ->
    m (HLIR.Type, [HLIR.TLIR "toplevel"])
maybeResolveEnumeration depth name args = do
    specState <- liftIO $ readIORef defaultSpecializer

    case Map.lookup name specState.enumerations of
        Just scheme@(HLIR.Forall orderedVars' instMap) -> do
            -- Instantiating the scheme to get a concrete type and a substitution
            -- that maps the quantified variables to concrete types.
            (_, sub) <- M.instantiateMapWithSub scheme

            -- Ensuring the number of type arguments matches the number of
            -- type variables in the enumeration definition
            when (Map.size sub /= length args)
                $ M.throw (M.InvalidArgumentQuantity (Map.size instMap) (length args))

            -- Creating a substitution map that maps the enumeration's type variables
            -- to the provided type arguments
            let s = Map.fromList (zip orderedVars' args) <> sub

            -- Creating a unique name for the specialized enumeration
            -- This is done by re-ordering the scheme variables and using the
            -- substitution `s` so the naming is consistent with other places
            -- that generate specialization names (avoids mismatched naming)
            let orderedVars = flip map orderedVars' $ \var -> Map.findWithDefault (HLIR.MkTyQuantified var) var s
            specName <- HLIR.prettify (HLIR.MkTyApp (HLIR.MkTyId name) orderedVars)

            -- Checking if we have already created this specialization
            -- to avoid duplicating work
            -- If it exists, we simply return the specialized type
            -- If it does not exist, we create a new enumeration declaration
            -- and update the specializer state accordingly
            if Set.member specName specState.rememberedEnumerations
                then pure (HLIR.MkTyId specName, [])
                else do
                    modifyIORef' defaultSpecializer $ \s' ->
                        s'{rememberedEnumerations = Set.insert specName s'.rememberedEnumerations}

                    -- Applying the substitution to each constructor type in the enumeration
                    -- and recursively resolving specializations in those types
                    -- This ensures that nested specializations are also handled correctly
                    -- The result is a list of specialized constructors and any new toplevel definitions
                    -- that were created during the specialization process
                    (ns, specializedConstructors) <-
                        sequence
                            <$> Trav.for
                                (Map.toList instMap)
                                ( \(n, ty') -> do
                                    sTy <- M.applySubstitution s ty'
                                    (finalTy, ns') <- resolveSpecializationInType (depth + 1) sTy
                                    pure (ns', (n <> "@" <> Text.intercalate "_" (map toText orderedVars), finalTy))
                                )

                    let fieldsWithTypeAsArray = flip Map.map (Map.fromList specializedConstructors) $ \case
                            (tys HLIR.:->: _) -> Just tys
                            _ -> Nothing

                    -- Creating the new specialized enumeration declaration
                    -- with the new name and specialized constructors
                    -- This declaration is added to the list of new toplevel definitions
                    -- that will be returned
                    let newEnum =
                            HLIR.MkTopEnumeration
                                { HLIR.name = HLIR.MkAnnotation specName []
                                , HLIR.constructors = fieldsWithTypeAsArray
                                }

                    -- Updating the specializer state to include the new enumeration
                    -- and marking it as remembered to avoid future duplication
                    -- This is crucial for maintaining the integrity of the specialization process
                    -- and ensuring that subsequent requests for the same specialization
                    -- are handled efficiently

                    modifyIORef' defaultSpecializer $ \s' ->
                        s'
                            { enumerations =
                                Map.insert
                                    specName
                                    (HLIR.Forall [] (Map.fromList specializedConstructors))
                                    s'.enumerations
                            }

                    pure (HLIR.MkTyId specName, ns ++ [newEnum])
        Nothing -> do
            -- Creating the type header based on whether there are arguments or not
            -- (e.g., MyStruct or MyStruct<T1, T2>)
            let header
                    | null args = HLIR.MkTyId name
                    | otherwise = HLIR.MkTyApp (HLIR.MkTyId name) args

            -- If the enumeration is not found, we assume it's a regular type
            -- and return it as is.
            pure (header, [])

-- | Utility types
data Specializer = Specializer
    { variables :: Map Text (HLIR.Scheme HLIR.Type, Maybe (HLIR.TLIR "toplevel"))
    , structures :: Map Text (HLIR.Scheme (Map Text HLIR.Type))
    , implementations :: Map (Text, HLIR.Scheme HLIR.Type) (Maybe (HLIR.TLIR "toplevel"))
    , properties :: Map Text (HLIR.Scheme HLIR.Type)
    , enumerations :: Map Text (HLIR.Scheme (Map Text HLIR.Type))
    , rememberedVariables :: Set Text
    , rememberedStructures :: Set Text
    , rememberedImplementations :: Set Text
    , rememberedNatives :: Set Text
    , rememberedEnumerations :: Set Text
    , rememberedLocals :: Set Text
    , paramLessConstructors :: Set Text
    }
    deriving (Ord, Eq)

emptySpecializer :: Specializer
emptySpecializer =
    Specializer
        Map.empty
        Map.empty
        Map.empty
        Map.empty
        Map.empty
        Set.empty
        Set.empty
        Set.empty
        Set.empty
        Set.empty
        Set.empty
        Set.empty

defaultSpecializer :: IORef Specializer
defaultSpecializer = IO.unsafePerformIO $ newIORef emptySpecializer

withLocals :: (MonadIO m) => Set Text -> m a -> m a
withLocals locals action = do
    old <- readIORef defaultSpecializer
    modifyIORef' defaultSpecializer $ \s -> s{rememberedLocals = locals <> old.rememberedLocals}
    result <- action
    modifyIORef' defaultSpecializer $ \s -> s{rememberedLocals = old.rememberedLocals}
    pure result
