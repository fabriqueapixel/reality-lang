{-# LANGUAGE LambdaCase #-}

module Language.Reality.Frontend.Typechecker.Checker where

import Control.Color (printText)
import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Foldable qualified as List
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Traversable qualified as List
import GHC.IO qualified as IO
import Language.Reality.Backend.ANF.Converter (extractArgumentsAndReturnType)
import Language.Reality.Frontend.Parser.Internal.Type (buildTypeApp)
import Language.Reality.Frontend.Typechecker.Monad qualified as M
import Language.Reality.Frontend.Typechecker.Unification qualified as M
import Language.Reality.Syntax.HLIR qualified as HLIR

replacePatternByVars ::
  (MonadIO m, M.MonadError M.Error m) =>
  HLIR.HLIR "pattern" ->
  HLIR.HLIR "pattern" ->
  m (HLIR.HLIR "pattern")
replacePatternByVars (HLIR.MkPatternConstructor name xs _) (HLIR.MkPatternConstructor name' xs' _)
  | name == name' = do
      xs'' <- zipWithM replacePatternByVars xs xs'
      pure (HLIR.MkPatternConstructor name xs'' Nothing)
  | otherwise = pure (HLIR.MkPatternConstructor name xs Nothing)
replacePatternByVars (HLIR.MkPatternVariable ann) (HLIR.MkPatternVariable ann')
  | ann.name == ann'.name = do
      pure (HLIR.MkPatternVariable ann)
replacePatternByVars p1@(HLIR.MkPatternVariable _) (HLIR.MkPatternConstructor {}) = do
  pure p1
replacePatternByVars (HLIR.MkPatternConstructor {}) p2@(HLIR.MkPatternVariable _) = do
  pure p2
replacePatternByVars p1@(HLIR.MkPatternLiteral l1) (HLIR.MkPatternLiteral l2)
  | l1 == l2 = do
      pure p1
  | otherwise = M.throw (M.CompilerError "Pattern literals do not match.")
replacePatternByVars (HLIR.MkPatternVariable ann) (HLIR.MkPatternLet _) = do
  pure (HLIR.MkPatternVariable ann)
replacePatternByVars (HLIR.MkPatternLocated _ p1) p2 = do
  replacePatternByVars p1 p2
replacePatternByVars p1 (HLIR.MkPatternLocated _ p2) = do
  replacePatternByVars p1 p2
replacePatternByVars (HLIR.MkPatternLet _) p2@(HLIR.MkPatternLet _) = do
  pure p2
replacePatternByVars (HLIR.MkPatternReference binding _) (HLIR.MkPatternReference binding' t') = do
  p <- replacePatternByVars binding binding'
  pure $ HLIR.MkPatternReference p t'
replacePatternByVars (HLIR.MkPatternReference _ t) p = do
  pure (HLIR.MkPatternReference p t)
replacePatternByVars p1 p2 =
  M.throw $ M.CompilerError $ "Cannot replace pattern " <> toText p1 <> " with pattern " <> toText p2

mapAndUnzip4M :: (Monad m) => (a -> m (b, c, d, e)) -> [a] -> m ([b], [c], [d], [e])
mapAndUnzip4M f xs = do
  results <- mapM f xs
  let (bs, cs, ds, es) = List.unzip4 results
  pure (bs, cs, ds, es)

getAllExternalFunctions :: [HLIR.HLIR "toplevel"] -> [(Text, HLIR.Scheme HLIR.Type)]
getAllExternalFunctions =
  List.foldl'
    ( \acc toplevel ->
        case toplevel of
          HLIR.MkTopExternalFunction ann typeValue ->
            (ann.name, HLIR.Forall ann.typeValue typeValue) : acc
          HLIR.MkTopLocated _ n -> getAllExternalFunctions [n] ++ acc
          HLIR.MkTopPublic n -> getAllExternalFunctions [n] ++ acc
          _ -> acc
    )
    []

withTypeVars ::
  (MonadIO m) =>
  [Text] ->
  m a ->
  m a
withTypeVars typeVars action = do
  oldState <- readIORef M.defaultCheckerState

  -- Adding type variables to the state
  modifyIORef' M.defaultCheckerState $ \s ->
    s {M.typeVariables = Set.union (Set.fromList typeVars) s.typeVariables}

  result <- action

  -- Restoring old state
  modifyIORef' M.defaultCheckerState $ \s ->
    s {M.typeVariables = oldState.typeVariables}

  pure result

-- | TYPECHECKER
-- | Typecheck a program.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with types checked.
runTypechecker ::
  (MonadIO m, M.MonadError M.Error m) =>
  [HLIR.HLIR "toplevel"] ->
  m [HLIR.TLIR "toplevel"]
runTypechecker toplevels = do
  M.resetState

  allFunctionSignatures <- getAllFunctionSignatures toplevels
  let allExternalFunctions = getAllExternalFunctions toplevels

  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment = allFunctionSignatures `List.union` s.environment,
        M.properties = Map.fromList [(name, scheme) | (name, scheme) <- allExternalFunctions] `Map.union` s.properties
      }

  xs <- concat <$> mapM checkToplevelSingular toplevels

  isThereMain <- liftIO $ readIORef noMain
  case isThereMain of
    Just (fnName, fnType@(_ HLIR.:->: ret)) -> do
      let entryCall =
            HLIR.MkExprApplication
              ( HLIR.MkExprVariable
                  (HLIR.MkAnnotation fnName (Identity fnType))
                  []
              )
              []
              (Identity ret)

      let body =
            HLIR.MkExprLetIn
              (HLIR.MkAnnotation "exit_code" (Identity HLIR.MkTyInt))
              entryCall
              (HLIR.MkExprLiteral (HLIR.MkLitInt 0))
              (Identity HLIR.MkTyInt)

      let mainFun =
            HLIR.MkTopFunctionDeclaration
              (HLIR.MkAnnotation "main" [])
              [HLIR.MkAnnotation "args" (HLIR.MkTyList (HLIR.MkTyId "String"))]
              HLIR.MkTyInt
              body

      pure (xs ++ [mainFun])
    _ -> pure xs

isArgsAnnotation :: [HLIR.Annotation HLIR.Type] -> Bool
isArgsAnnotation (x : _) | x.name == "args" && x.typeValue == HLIR.MkTyList (HLIR.MkTyId "String") = True
isArgsAnnotation _ = False

getAllFunctionSignatures ::
  (MonadIO m, M.MonadError M.Error m) =>
  [HLIR.HLIR "toplevel"] ->
  m [(Text, HLIR.Scheme HLIR.Type)]
getAllFunctionSignatures toplevels = do
  let extractFunctionSignature :: HLIR.HLIR "toplevel" -> Maybe (Text, HLIR.Scheme HLIR.Type)
      extractFunctionSignature (HLIR.MkTopFunctionDeclaration ann params ret _) =
        Just
          ( ann.name,
            HLIR.Forall ann.typeValue (map (.typeValue) params HLIR.:->: ret)
          )
      extractFunctionSignature (HLIR.MkTopLocated _ n) = extractFunctionSignature n
      extractFunctionSignature (HLIR.MkTopPublic n) = extractFunctionSignature n
      extractFunctionSignature (HLIR.MkTopConstantDeclaration ann _) =
        Just
          ( ann.name,
            HLIR.Forall [] ann.typeValue
          )
      extractFunctionSignature _ = Nothing

  let functionSignatures = mapMaybe extractFunctionSignature toplevels

  pure functionSignatures

getAnnotationName :: HLIR.HLIR "expression" -> Maybe Text
getAnnotationName (HLIR.MkExprVariable ann _) = Just ann.name
getAnnotationName (HLIR.MkExprLocated _ e) = getAnnotationName e
getAnnotationName _ = Nothing

getAnnotations :: HLIR.HLIR "toplevel" -> ([Text], HLIR.HLIR "toplevel", [HLIR.HLIR "expression"])
getAnnotations (HLIR.MkTopAnnotation args node) =
  let (anns, n, exprs) = getAnnotations node
      annNames = mapMaybe getAnnotationName args
   in (annNames ++ anns, n, exprs ++ args)
getAnnotations (HLIR.MkTopLocated p n) =
  let (anns, n', exprs) = getAnnotations n
   in (anns, HLIR.MkTopLocated p n', exprs)
getAnnotations (HLIR.MkTopPublic n) =
  let (anns, n', exprs) = getAnnotations n
   in (anns, HLIR.MkTopPublic n', exprs)
getAnnotations n = ([], n, [])

noMain :: IORef (Maybe (Text, HLIR.Type))
noMain = IO.unsafePerformIO $ newIORef Nothing
{-# NOINLINE noMain #-}

getFunctionNode :: HLIR.HLIR "toplevel" -> Maybe (HLIR.HLIR "toplevel")
getFunctionNode n@(HLIR.MkTopFunctionDeclaration {}) = Just n
getFunctionNode (HLIR.MkTopLocated _ n) = getFunctionNode n
getFunctionNode (HLIR.MkTopPublic n) = getFunctionNode n
getFunctionNode _ = Nothing

-- | Typecheck a singular HLIR toplevel node.
-- | This function takes a toplevel node, and returns a toplevel node with types
-- | checked.
-- | Toplevel checking is mainly about modifying the environment with new
-- | definitions, and checking function bodies.
checkToplevelSingular ::
  (MonadIO m, M.MonadError M.Error m) =>
  HLIR.HLIR "toplevel" ->
  m [HLIR.TLIR "toplevel"]
checkToplevelSingular e
  | (anns, node, eAnns) <- getAnnotations e,
    not (null anns),
    Just (HLIR.MkTopFunctionDeclaration ann args ret _) <- getFunctionNode node =
      do
        typedNodes <- checkToplevelSingular node

        eAnns' <-
          mapM
            ( \expr -> do
                (_, typedExpr, _, _) <- synthesizeE expr
                pure typedExpr
            )
            eAnns

        if "main_entry" `elem` anns
          then do
            let funcType = map (.typeValue) args HLIR.:->: ret
            liftIO $ writeIORef noMain (Just (ann.name, funcType))
            pure (HLIR.MkTopAnnotation eAnns' <$> typedNodes)
          else
            pure (HLIR.MkTopAnnotation eAnns' <$> typedNodes)
checkToplevelSingular (HLIR.MkTopConstantDeclaration ann expr) = do
  -- Removing aliases from the expected type
  expectedType <- M.performAliasRemoval ann.typeValue

  -- Checking the expression against the expected type
  (typedExpr, cs, _) <- checkE expectedType expr

  -- Expression should not have any unresolved constraints as this
  -- means that there are function calls. But constants cannot have function calls.
  -- So we throw an error if there are any unresolved constraints.
  unless (null cs) $
    M.throw (M.UnsolvedConstraints cs)

  -- Adding the constant to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment = (ann.name, HLIR.Forall [] expectedType) : s.environment
      }

  pure [HLIR.MkTopConstantDeclaration ann typedExpr]
checkToplevelSingular (HLIR.MkTopPatternAlias binding retType arguments pattern) = do
  -- Removing aliases from the expected types of the arguments
  expectedTypes <- mapM (M.performAliasRemoval . (.typeValue)) arguments
  retType' <- M.performAliasRemoval retType

  let header = retType'
  let funType = expectedTypes HLIR.:->: header

  -- Checking the pattern against the expected types
  (typedPattern, cs, _) <- checkP header pattern

  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.patternAliases =
          Map.insert binding.name (HLIR.Forall binding.typeValue funType, pattern) s.patternAliases
      }

  -- Pattern should not have any unresolved constraints as this
  -- means that there are function calls. But pattern aliases cannot have function calls.
  -- So we throw an error if there are any unresolved constraints.
  unless (null cs) $
    M.throw (M.UnsolvedConstraints cs)

  pure [HLIR.MkTopPatternAlias binding retType' arguments typedPattern]
checkToplevelSingular (HLIR.MkTopFunctionDeclaration ann params ret body) = withTypeVars ann.typeValue $ do
  -- Removing aliases from the parameter and return types
  paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
  retType <- M.performAliasRemoval ret

  -- Building new parameters and function type
  let newParams =
        if ann.name == "main"
          then
            [ HLIR.MkAnnotation "argc" HLIR.MkTyInt,
              HLIR.MkAnnotation
                "argv"
                (HLIR.MkTyPointer (HLIR.MkTyPointer HLIR.MkTyChar))
            ]
          else zipWith (\p ty -> p {HLIR.typeValue = ty}) params paramTypes
      funcType = paramTypes HLIR.:->: retType

  let argListType = HLIR.MkTyList (HLIR.MkTyId "String")
      argc = HLIR.MkExprVariable (HLIR.MkAnnotation "argc" (Just HLIR.MkTyInt)) []
      argv =
        HLIR.MkExprVariable
          ( HLIR.MkAnnotation
              "argv"
              (Just (HLIR.MkTyPointer (HLIR.MkTyPointer HLIR.MkTyChar)))
          )
          []
      getArgsType =
        [HLIR.MkTyInt]
          HLIR.:->: ( [HLIR.MkTyPointer (HLIR.MkTyPointer HLIR.MkTyChar)]
                        HLIR.:->: argListType
                    )

  let body'
        | ann.name == "main" && isArgsAnnotation params =
            HLIR.MkExprLetIn
              (HLIR.MkAnnotation "args" (Just argListType))
              ( HLIR.MkExprApplication
                  ( HLIR.MkExprApplication
                      ( HLIR.MkExprVariable
                          (HLIR.MkAnnotation "get_args" (Just getArgsType))
                          []
                      )
                      [argc]
                      Nothing
                  )
                  [argv]
                  Nothing
              )
              body
              Nothing
        | otherwise = body

  -- Adding the function to the environment, before adding arguments,
  -- to allow for restoration of the environment without deleting the
  -- function itself.
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment =
          (ann.name, HLIR.Forall ann.typeValue funcType) : s.environment
      }

  -- Collecting old environment to restore it later
  oldEnv <- readIORef M.defaultCheckerState

  -- Adding parameters to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment =
          foldr
            ( \p env ->
                (p.name, HLIR.Forall [] p.typeValue) : env
            )
            s.environment
            newParams,
        M.returnType = Just retType
      }

  -- Checking the function body against the return type
  (typedBody, cs, _) <- checkE retType body'

  -- Solving constraints generated during the body checking
  solveConstraints cs

  -- Restoring the old environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment = oldEnv.environment,
        M.returnType = oldEnv.returnType
      }

  pure [HLIR.MkTopFunctionDeclaration ann newParams retType typedBody]
checkToplevelSingular (HLIR.MkTopTypeAlias ann aliased) = withTypeVars ann.typeValue $ do
  -- Removing aliases from the aliased type
  realiasedType <- M.performAliasRemoval aliased

  -- Adding the type alias to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.typeAliases =
          Map.insert ann.name (HLIR.Forall ann.typeValue realiasedType) s.typeAliases
      }

  pure [HLIR.MkTopTypeAlias ann aliased]
checkToplevelSingular (HLIR.MkTopLocated p n) = do
  HLIR.pushPosition p
  typedNode <- checkToplevelSingular n
  void HLIR.popPosition
  pure $ map (HLIR.MkTopLocated p) typedNode
checkToplevelSingular (HLIR.MkTopPublic node) = do
  typedNode <- checkToplevelSingular node
  pure $ map HLIR.MkTopPublic typedNode
checkToplevelSingular (HLIR.MkTopModuleDeclaration {}) = M.throw (M.CompilerError "Modules are not supported in the typechecker.")
checkToplevelSingular (HLIR.MkTopExternalFunction ann typeValue) = do
  -- Removing aliases from the parameter and return types
  paramType <- M.performAliasRemoval typeValue

  -- Adding the external function to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment =
          (ann.name, HLIR.Forall ann.typeValue paramType) : s.environment
      }

  pure [HLIR.MkTopExternalFunction ann paramType]
checkToplevelSingular (HLIR.MkTopImport _) = M.throw (M.CompilerError "Imports are not supported in the typechecker.")
checkToplevelSingular (HLIR.MkTopProperty header propTypeValue) = withTypeVars header.typeValue $ do
  -- Removing aliases from the parameter and return types
  funcType <- M.performAliasRemoval propTypeValue

  -- Adding the property to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.properties =
          Map.insert header.name (HLIR.Forall header.typeValue funcType) s.properties
      }

  pure [HLIR.MkTopProperty header funcType]
checkToplevelSingular (HLIR.MkTopImplementation forType header params returnType body) = withTypeVars header.typeValue $ do
  -- Find the property being implemented
  scheme <- findPropertyByName header.name

  -- Removing aliases from the parameter, return and for types
  aliasedReturnType <- M.performAliasRemoval returnType
  aliasedParamTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
  aliasedForType <- M.performAliasRemoval forType.typeValue

  -- Building the implementation scheme
  let funcType = (aliasedForType : aliasedParamTypes) HLIR.:->: aliasedReturnType
      implScheme = HLIR.Forall header.typeValue funcType

  -- Checking if the implementation matches the property

  expectedPropType <- M.instantiate scheme >>= M.performAliasRemoval
  expectedImplType <- M.instantiate implScheme >>= M.performAliasRemoval

  void $ expectedImplType `M.isSubtypeOf` expectedPropType

  -- Adding the implementation to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.implementations =
          Map.insert (header.name, HLIR.Forall header.typeValue aliasedForType) implScheme s.implementations
      }

  -- Collecting old environment to restore it later
  paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
  let newParams = zipWith (\p ty -> p {HLIR.typeValue = ty}) params paramTypes

  -- Building the environment for checking the body
  let env =
        List.map ((HLIR.Forall [] <$>) . HLIR.unannotate) newParams
          ++ [(forType.name, HLIR.Forall [] aliasedForType)]

  oldEnv <- readIORef M.defaultCheckerState
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment = env <> s.environment,
        M.returnType = Just aliasedReturnType
      }

  -- Checking the function body against the return type
  -- We use `withEnvironment` to temporarily extend the environment
  -- with the parameters and for type while checking the body.
  (newBody, cs, _) <- checkE aliasedReturnType body

  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment = oldEnv.environment,
        M.returnType = oldEnv.returnType
      }

  -- Solving constraints generated during the body checking
  solveConstraints cs

  pure
    [ HLIR.MkTopImplementation
        forType {HLIR.typeValue = aliasedForType}
        header
        newParams
        aliasedReturnType
        newBody
    ]
checkToplevelSingular (HLIR.MkTopAnnotation args node) = do
  args' <- map (\(_, e, _, _) -> e) <$> forM args synthesizeE

  typedNode <- checkToplevelSingular node

  pure [HLIR.MkTopAnnotation args' n | n <- typedNode]
checkToplevelSingular (HLIR.MkTopExternLet ann) = do
  -- Removing aliases from the expected type
  expectedType <- M.performAliasRemoval ann.typeValue

  -- Adding the extern let to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment = List.insert (ann.name, HLIR.Forall [] expectedType) s.environment
      }

  pure [HLIR.MkTopExternLet ann]
checkToplevelSingular (HLIR.MkTopEnumeration ann constructors) = withTypeVars ann.typeValue $ do
  -- Removing aliases from the constructor types
  constructorTypes <-
    traverse
      ( \case
          Just ts -> Just <$> mapM M.performAliasRemoval ts
          Nothing -> pure Nothing
      )
      constructors

  -- Formatting the enumeration types
  -- Each constructor type is a function type that takes the
  -- constructor arguments and returns the enumeration type.
  -- For example, for `enum Option<T> { Some(T), None }`,
  -- the constructor `Some` has type `T -> Option<T>`,
  -- and `None` has type `Option<T>`.

  let enumType
        | null ann.typeValue = HLIR.MkTyId ann.name
        | otherwise =
            buildTypeApp (HLIR.MkTyId ann.name) (map HLIR.MkTyQuantified ann.typeValue)
      formatConstructor (cName, t) = (cName, buildConstructorType t)

      buildConstructorType :: Maybe [HLIR.Type] -> HLIR.Type
      buildConstructorType Nothing = enumType
      buildConstructorType (Just (t : ts)) = [t] HLIR.:->: buildConstructorType (Just ts)
      buildConstructorType (Just []) = enumType

  let formattedConstructors = map formatConstructor (Map.toList constructorTypes)

  -- Adding the enumeration to the environment
  -- Each constructor is added as a separate entry in the environment
  -- with its corresponding type.

  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment =
          foldr
            ( \(name, ty) env ->
                (name, HLIR.Forall ann.typeValue ty) : env
            )
            s.environment
            formattedConstructors
      }

  pure [HLIR.MkTopEnumeration ann constructorTypes]
checkToplevelSingular (HLIR.MkTopLet binding value) = do
  -- Removing aliases from the expected type
  expectedType <- maybe M.newType M.performAliasRemoval binding.typeValue

  -- Checking the value against the expected type
  (typedValue, cs, _) <- checkE expectedType value

  -- Expression should not have any unresolved constraints as this
  -- means that there are function calls. But let bindings cannot have function calls.
  -- So we throw an error if there are any unresolved constraints.
  unless (null cs) $
    M.throw (M.UnsolvedConstraints cs)

  -- Adding the let binding to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment = List.insert (binding.name, HLIR.Forall [] expectedType) s.environment
      }

  pure [HLIR.MkTopLet binding {HLIR.typeValue = Identity expectedType} typedValue]

synthesizeE ::
  (MonadIO m, M.MonadError M.Error m) =>
  HLIR.HLIR "expression" ->
  m (HLIR.Type, HLIR.TLIR "expression", M.Constraints, [(Text, HLIR.Scheme HLIR.Type)])
synthesizeE (HLIR.MkExprLocated p e) = do
  HLIR.pushPosition p

  (ty, expr, cs, b) <- synthesizeE e

  void HLIR.popPosition

  pure (ty, HLIR.MkExprLocated p expr, cs, b)
synthesizeE (HLIR.MkExprLiteral lit) = case lit of
  -- Literal values have known types, so we can directly return them
  -- along with their types and no constraints

  -- Int have type i32 by default
  HLIR.MkLitInt n -> pure (HLIR.MkTyInt, HLIR.MkExprLiteral (HLIR.MkLitInt n), mempty, mempty)
  -- Float have type f32 by default
  HLIR.MkLitFloat f -> pure (HLIR.MkTyFloat, HLIR.MkExprLiteral (HLIR.MkLitFloat f), mempty, mempty)
  -- Bool have type bool
  HLIR.MkLitBool b -> pure (HLIR.MkTyBool, HLIR.MkExprLiteral (HLIR.MkLitBool b), mempty, mempty)
  -- String have type *char
  HLIR.MkLitString s -> pure (HLIR.MkTyString, HLIR.MkExprLiteral (HLIR.MkLitString s), mempty, mempty)
  -- Char have type char
  HLIR.MkLitChar c -> pure (HLIR.MkTyChar, HLIR.MkExprLiteral (HLIR.MkLitChar c), mempty, mempty)
synthesizeE (HLIR.MkExprVariable ann types) = do
  -- Looking up the variable in the environment
  env <- readIORef M.defaultCheckerState
  let variables = env.environment
      properties = env.properties

  case List.lookup ann.name variables of
    Just scheme -> do
      -- If variable is found, instantiate its type scheme, and
      -- remove aliases from the instantiated type
      ty <- M.instantiateWithSub scheme types >>= M.performAliasRemoval

      pure (ty, HLIR.MkExprVariable ann {HLIR.typeValue = Identity ty} types, mempty, mempty)
    Nothing -> case Map.lookup ann.name properties of
      Just scheme -> do
        -- If variable is a property, we treat it as a regular
        -- variable, but we also add a constraint that it must
        -- be implemented for the given types
        ty <- M.instantiateWithSub scheme types >>= M.performAliasRemoval
        pos <- HLIR.peekPosition'
        pure
          ( ty,
            HLIR.MkExprVariable ann {HLIR.typeValue = Identity ty} types,
            [M.MkImplConstraint ann.name ty pos types],
            mempty
          )
      Nothing -> M.throw (M.VariableNotFound ann.name Nothing)
synthesizeE (HLIR.MkExprCondition cond thenB elseB _ _) = do
  -- Condition must be of type bool
  (condExpr, cs1, b1) <- checkE HLIR.MkTyBool cond

  -- Then and else branches must have the same type
  -- We first synthesize the type of the then branch,
  -- then check the else branch against that type
  (thenTy, thenExpr, cs2, _) <- M.withEnvironment b1 $ synthesizeE thenB
  (elseExpr, cs3, _) <- checkE thenTy elseB

  -- Collecting all constraints
  let cs = cs1 <> cs2 <> cs3

  pure
    (thenTy, HLIR.MkExprCondition condExpr thenExpr elseExpr (Identity thenTy) (Identity thenTy), cs, mempty)
synthesizeE (HLIR.MkExprLetIn binding value inExpr _) = do
  -- Collecting old environment to restore it later
  oldEnv <- readIORef M.defaultCheckerState <&> M.environment

  -- If the binding has a type annotation, we use it as the expected type
  -- and remove aliases from it. Otherwise, we create a new type variable
  expectedType <- maybe M.newType M.performAliasRemoval binding.typeValue

  -- Adding the binding to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment =
          (binding.name, HLIR.Forall [] expectedType) : s.environment
      }

  -- Checking the value against the expected type
  (valueExpr, cs1, _) <- checkE expectedType value

  -- Ensuring that the type of the binding is a subtype of the expected type
  forM_ binding.typeValue $ \t ->
    void $ t `M.isSubtypeOf` expectedType

  -- Synthesizing the type of the in expression
  -- The type of the in expression is the type of the whole let-in expression
  -- so we return it as the type of the let-in expression
  (inTy, typedInExpr, cs2, _) <- synthesizeE inExpr

  -- Restoring the old environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s {M.environment = oldEnv}

  -- Collecting all constraints
  let cs = cs1 <> cs2

  pure
    ( inTy,
      HLIR.MkExprLetIn
        binding {HLIR.typeValue = Identity expectedType}
        valueExpr
        typedInExpr
        (Identity inTy),
      cs,
      mempty
    )
synthesizeE (HLIR.MkExprLambda params ret body) = do
  -- Removing aliases from the parameter and return types
  paramTypes <- mapM (maybe M.newType M.performAliasRemoval . (.typeValue)) params
  retType <- maybe M.newType M.performAliasRemoval ret

  -- Building parameter annotations with resolved types
  let paramAnnotations = zipWith (\p ty -> p {HLIR.typeValue = Identity ty}) params paramTypes

  -- Collecting old environment to restore it later
  oldEnv <- readIORef M.defaultCheckerState

  -- Adding parameters to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment =
          foldr
            ( \p env ->
                (p.name, HLIR.Forall [] p.typeValue.runIdentity) : env
            )
            s.environment
            paramAnnotations,
        M.returnType = Just retType
      }

  -- Checking the body against the return type
  (bodyExpr, cs, _) <- checkE retType body

  -- Restoring the old environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment = oldEnv.environment,
        M.returnType = oldEnv.returnType
      }

  -- Building the function type
  let funcType = paramTypes HLIR.:->: retType

  pure
    (funcType, HLIR.MkExprLambda paramAnnotations (Identity retType) bodyExpr, cs, mempty)
synthesizeE (HLIR.MkExprApplication fun args _) | Just funName <- getVariable fun = do
  -- Synthesizing the type of the callee
  (argsTys, argsExprs, cs1, b) <- mapAndUnzip4M synthesizeE args

  retType <- M.newType

  -- Looking up the function in the environment
  state' <- readIORef M.defaultCheckerState
  let env = state'.environment

  pos <- HLIR.peekPosition'

  (funcType, _) <-
    findImplementationMatching env [] funName (argsTys HLIR.:->: retType) pos

  case funcType of
    Just func -> do
      -- Collecting all constraints
      let cs = concat cs1

      void $ func `M.isSubtypeOf` (argsTys HLIR.:->: retType)

      pure
        ( retType,
          HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation funName (Identity func)) []) argsExprs (Identity retType),
          cs,
          mconcat b
        )
    Nothing -> do
      properties <- readIORef M.defaultCheckerState <&> M.properties

      case Map.lookup funName properties of
        Just scheme -> do
          -- If the function is a property, we treat it as a regular
          -- variable, but we also add a constraint that it must
          -- be implemented for the given types
          ty <- M.instantiateWithSub scheme [] >>= M.performAliasRemoval

          void $ ty `M.isSubtypeOf` (argsTys HLIR.:->: retType)

          let cs = concat cs1

          pure
            ( retType,
              HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation funName (Identity ty)) []) argsExprs (Identity retType),
              cs <> [M.MkImplConstraint funName ty pos []],
              mconcat b
            )
        Nothing -> M.throw (M.VariableNotFound funName (Just (argsTys HLIR.:->: retType)))
synthesizeE (HLIR.MkExprApplication callee args _) = do
  -- Synthesizing the type of the callee
  (calleeTy, calleeExpr, cs1, b) <- synthesizeE callee

  case calleeTy of
    HLIR.MkTyFun paramTypes retType -> do
      -- If the callee is a function type, we check that the number of
      -- arguments matches the number of parameters.
      if length paramTypes /= length args
        then M.throw (M.InvalidArgumentQuantity (length paramTypes) (length args))
        else do
          -- Checking each argument against the corresponding parameter type
          -- and collecting constraints from each argument.
          (checkedArgs, cs2, bs) <-
            List.foldlM
              ( \(accArgs, accCs, accBs) (paramTy, argExpr) -> do
                  (checkedArg, csArg, bArg) <-
                    M.withEnvironment accBs $
                      checkE paramTy argExpr
                  pure (accArgs ++ [checkedArg], accCs <> csArg, accBs <> bArg)
              )
              ([], [], mempty)
              (zip paramTypes args)

          -- Collecting all constraints
          let cs = cs1 <> cs2

          pure
            (retType, HLIR.MkExprApplication calleeExpr checkedArgs (Identity retType), cs, bs <> b)
    _ -> do
      -- If the callee is not a function type, we create new type variables
      newParamTypes <- mapM (const M.newType) args
      newRetType <- M.newType

      -- We expect the callee to be a function type with the new parameter
      -- types and new return type.
      let expectedCalleeType = HLIR.MkTyFun newParamTypes newRetType

      -- Checking each argument against the new parameter types
      -- and collecting constraints from each argument.
      (checkedArgs, cs2, bs) <- unzip3 <$> zipWithM checkE newParamTypes args

      -- We add a constraint that the callee type must be a subtype of
      -- the expected function type.
      void $ calleeTy `M.isSubtypeOf` expectedCalleeType

      -- Collecting all constraints
      let cs = cs1 <> mconcat cs2

      pure
        ( newRetType,
          HLIR.MkExprApplication calleeExpr checkedArgs (Identity newRetType),
          cs,
          mconcat bs <> b
        )
synthesizeE (HLIR.MkExprStructureCreation field value record _ _) = do
  (ty, e', cs1, bs1) <- synthesizeE record
  (vTy, v', cs2, bs2) <- synthesizeE value

  a <- M.newType
  r <- M.newType

  let funTy = [a, HLIR.MkTyRecord r] HLIR.:->: HLIR.MkTyRecord (HLIR.MkTyRowExtend field a False r)

  ret <- M.newType

  void $ ([vTy, ty] HLIR.:->: ret) `M.isSubtypeOf` funTy

  pure (ret, HLIR.MkExprStructureCreation field v' e' (Identity vTy) (Identity r), cs1 <> cs2, bs1 <> bs2)
synthesizeE (HLIR.MkExprStructureAccess struct field) = do
  (ty, e', cs1, bs1) <- synthesizeE struct

  a <- M.newType
  r <- M.newType

  let funTy = [HLIR.MkTyRecord (HLIR.MkTyRowExtend field a False r)] HLIR.:->: a

  ret <- M.newType

  void $ ([ty] HLIR.:->: ret) `M.isSubtypeOf` funTy

  pure (ret, HLIR.MkExprStructureAccess e' field, cs1, bs1)
synthesizeE (HLIR.MkExprDereference e _) = do
  -- Synthesizing the type of the expression to dereference
  -- It must be a pointer type. We also create a fresh type variable
  -- to represent the type being pointed to.
  newType <- M.newType
  (eTy, eExpr, cs, b) <- synthesizeE e

  -- The expected type is a pointer to the new type variable
  let expectedType = HLIR.MkTyPointer newType

  -- Adding a constraint that the expression type must be a subtype
  -- of the expected pointer type.
  void $ eTy `M.isSubtypeOf` expectedType

  pure (newType, HLIR.MkExprDereference eExpr (Identity newType), cs, b)
synthesizeE (HLIR.MkExprReference e _) = do
  -- Synthesizing the type of the expression to reference
  -- The resulting type is a pointer to that type.
  (eTy, eExpr, cs, b) <- synthesizeE e
  let refType = HLIR.MkTyPointer eTy

  pure (refType, HLIR.MkExprReference eExpr (Identity refType), cs, b)
synthesizeE (HLIR.MkExprUpdate update value _) = do
  -- Synthesizing the type of the update expression
  -- The update expression must be of a type that can be updated,
  -- so we just synthesize its type and use it as the expected type
  -- for the value expression.
  (updateTy, updateExpr, cs1, b1) <- synthesizeE update
  (valueExpr, cs2, b2) <- checkE updateTy value

  -- Collecting all constraints
  let cs = cs1 <> cs2

  pure (updateTy, HLIR.MkExprUpdate updateExpr valueExpr (Identity updateTy), cs, b1 <> b2)
synthesizeE (HLIR.MkExprSizeOf t) = do
  -- Sizeof expressions have type u64, and we just need to
  -- remove aliases from the type being measured.
  aliasedType <- M.performAliasRemoval t

  pure (HLIR.MkTyId "int", HLIR.MkExprSizeOf aliasedType, mempty, mempty)
synthesizeE (HLIR.MkExprSingleIf cond thenBranch _) = do
  -- Condition must be of type bool
  (condExpr, cs, b1) <- checkE HLIR.MkTyBool cond

  (thenTy, typedThenBranch, cs2, _) <- M.withEnvironment b1 $ synthesizeE thenBranch

  -- The type of the single-if expression is the type of the then branch
  pure
    ( thenTy,
      HLIR.MkExprSingleIf condExpr typedThenBranch (Identity thenTy),
      cs <> cs2,
      mempty
    )
synthesizeE (HLIR.MkExprCast expr targetTy) = do
  -- Synthesizing the type of the expression to cast
  (exprTy, exprExpr, cs, b) <- synthesizeE expr

  -- Removing aliases from the target type
  aliasedTargetTy <- M.performAliasRemoval targetTy

  -- Adding a constraint that the expression type must be a subtype
  -- of the target type.
  void $ exprTy `M.isSubtypeOf` aliasedTargetTy

  pure (aliasedTargetTy, HLIR.MkExprCast exprExpr aliasedTargetTy, cs, b)
synthesizeE (HLIR.MkExprWhile cond body _ inExpr) = do
  -- Condition must be of type bool
  (condExpr, cs1, b1) <- checkE HLIR.MkTyBool cond

  -- Synthesizing the type of the body
  (bodyTy, bodyExpr, cs2, _) <- M.withEnvironment b1 $ synthesizeE body

  -- Synthesizing the type of the in expression
  (inTy, inExprTyped, cs3, b3) <- synthesizeE inExpr

  -- Collecting all constraints
  let cs = cs1 <> cs2 <> cs3

  -- The type of the while expression is the type of the in expression
  pure
    (inTy, HLIR.MkExprWhile condExpr bodyExpr (Identity bodyTy) inExprTyped, cs, b3)
synthesizeE (HLIR.MkExprReturn e) = do
  -- Synthesizing the type of the return expression
  (eTy, eExpr, cs, bindings) <- synthesizeE e

  -- Finding the expected return type from the environment
  state' <- readIORef M.defaultCheckerState
  let expectedRetTy = state'.returnType

  case expectedRetTy of
    Just retTy -> do
      -- Adding a constraint that the return expression type must be
      -- a subtype of the expected return type.
      void $ eTy `M.isSubtypeOf` retTy

      pure (retTy, HLIR.MkExprReturn eExpr, cs, bindings)
    Nothing -> M.throw M.ReturnOutsideFunction
synthesizeE HLIR.MkExprBreak = do
  freshType <- M.newType

  -- Break expressions have type unit
  -- We just return unit type and no constraints
  pure (freshType, HLIR.MkExprBreak, mempty, mempty)
synthesizeE HLIR.MkExprContinue = do
  freshType <- M.newType

  -- Continue expressions have type unit
  -- We just return unit type and no constraints
  pure (freshType, HLIR.MkExprContinue, mempty, mempty)
synthesizeE (HLIR.MkExprIs e p _) = do
  -- Synthesizing the type of the expression to match
  (eTy, eExpr, cs, bindings1) <- synthesizeE e

  -- Checking the pattern against the expression type
  (typedPat, csPat, bindings2) <- checkP eTy p

  -- The type of the is expression is bool
  pure
    ( HLIR.MkTyBool,
      HLIR.MkExprIs eExpr typedPat (Identity eTy),
      cs <> csPat,
      bindings1 <> bindings2
    )
synthesizeE (HLIR.MkExprLetPatternIn {}) = M.throw (M.CompilerError "Let-pattern expressions are not supported in the typechecker.")
synthesizeE HLIR.MkExprStructureEmpty = do
  -- Empty structure expressions have type record {}, and we just return that
  pure (HLIR.MkTyRecord HLIR.MkTyRowEmpty, HLIR.MkExprStructureEmpty, mempty, mempty)
synthesizeE HLIR.MkExprNull = do
  -- Null expressions have type *void, and we just return that
  pure (HLIR.MkTyId "never", HLIR.MkExprNull, mempty, mempty)
synthesizeE (HLIR.MkExprLetValueless binding inExpr) = do
  typeValue <- M.performAliasRemoval binding.typeValue

  -- Collecting old environment to restore it later
  oldEnv <- readIORef M.defaultCheckerState <&> M.environment

  -- Adding the binding to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment =
          (binding.name, HLIR.Forall [] typeValue) : s.environment
      }

  -- Synthesizing the type of the in expression
  (inTy, typedInExpr, cs, _) <- synthesizeE inExpr

  -- Restoring the old environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s {M.environment = oldEnv}

  pure
    ( inTy,
      HLIR.MkExprLetValueless binding typedInExpr,
      cs,
      mempty
    )

-- | CHECK PATTERN
-- | Check a pattern against an expected type.
-- | This function returns the typed pattern and any constraints generated
-- | during the checking.
-- | It also extends the environment with any new bindings introduced by
-- | the pattern.
checkP ::
  (MonadIO m, M.MonadError M.Error m) =>
  HLIR.Type ->
  HLIR.HLIR "pattern" ->
  m (HLIR.TLIR "pattern", M.Constraints, [(Text, HLIR.Scheme HLIR.Type)])
checkP expected (HLIR.MkPatternLocated p pat) = do
  HLIR.pushPosition p

  (typedPat, cs, bindings) <- checkP expected pat

  void HLIR.popPosition

  pure (HLIR.MkPatternLocated p typedPat, cs, bindings)
checkP _ HLIR.MkPatternWildcard = do
  -- Wildcard patterns match any type, so we just return the expected type
  pure (HLIR.MkPatternWildcard, mempty, mempty)
checkP expected (HLIR.MkPatternLiteral lit) = do
  -- Literal patterns have known types, so we just check if the
  -- literal type is a subtype of the expected type
  let litType = case lit of
        HLIR.MkLitInt _ -> HLIR.MkTyInt
        HLIR.MkLitFloat _ -> HLIR.MkTyFloat
        HLIR.MkLitBool _ -> HLIR.MkTyBool
        HLIR.MkLitString _ -> HLIR.MkTyString
        HLIR.MkLitChar _ -> HLIR.MkTyChar

  void $ litType `M.isSubtypeOf` expected

  pure (HLIR.MkPatternLiteral lit, mempty, mempty)
checkP expected (HLIR.MkPatternVariable (HLIR.MkAnnotation name _)) = do
  -- We check if the variable type is a subtype of the expected type

  state' <- readIORef M.defaultCheckerState
  let env = state'.environment

  case List.lookup name env of
    Just scheme -> do
      varType <- M.instantiate scheme >>= M.performAliasRemoval

      void $ varType `M.isSubtypeOf` expected

      pure
        ( HLIR.MkPatternVariable (HLIR.MkAnnotation name (Identity varType)),
          mempty,
          mempty
        )
    Nothing -> M.throw (M.VariableNotFound name (Just expected))
checkP expected (HLIR.MkPatternStructure fields) = do
  -- We check if the pattern fields match the expected record type
  -- We first get all fields from the expected type, and then check
  -- if each pattern field matches the corresponding expected field

  (expectedFields, row) <- getAllFields expected

  case row of
    HLIR.MkTyRowEmpty -> do
      -- If the expected type is a closed record, we check that all
      -- pattern fields are present in the expected fields, and that
      -- their types match.
      (typedFields, cs, bindings) <-
        unzip3
          <$> forM
            (Map.toList fields)
            ( \(fieldName, fieldPat) -> do
                case Map.lookup fieldName expectedFields of
                  Just fieldType -> do
                    (typedPat, csPat, bindingsPat) <- checkP fieldType fieldPat
                    pure ((fieldName, typedPat), csPat, bindingsPat)
                  Nothing -> M.throw (M.FieldNotFound fieldName)
            )

      pure (HLIR.MkPatternStructure (Map.fromList typedFields), mconcat cs, mconcat bindings)
    _ -> M.throw (M.ExpectedRecord expected)
checkP expected p@(HLIR.MkPatternConstructor name constructors _) = do
  -- Pattern constructors acts like function calls in patterns.
  -- We find the constructor in the environment, instantiate its type,
  -- and check if the resulting type is a subtype of the expected type.

  state' <- readIORef M.defaultCheckerState
  let env = state'.environment
  let patternAliases = state'.patternAliases

  case Map.lookup name patternAliases of
    Just (_, pat)
      | HLIR.MkPatternConstructor name' xs _ <- removePatternLocation pat -> do
          ps <- zipWithM replacePatternByVars xs constructors

          checkP expected (HLIR.MkPatternConstructor name' ps Nothing)
    _ ->
      case List.lookup name env of
        Just scheme -> do
          ctorType <- M.instantiate scheme >>= M.performAliasRemoval

          let (args, ret) = extractArgumentsAndReturnType ctorType

          (pats, cs, bindings) <- unzip3 <$> zipWithM checkP args constructors

          void $ ret `M.isSubtypeOf` expected

          pure
            ( HLIR.MkPatternConstructor name pats (Identity ctorType),
              concat cs,
              List.concat bindings
            )
        Nothing -> M.throw (M.VariableNotFound name (Just expected))
checkP expected (HLIR.MkPatternLet (HLIR.MkAnnotation name _)) = do
  -- We treat let patterns as variable declaring patterns.
  pure
    ( HLIR.MkPatternLet (HLIR.MkAnnotation name (Identity expected)),
      mempty,
      [(name, HLIR.Forall [] expected)]
    )
checkP expected (HLIR.MkPatternReference pat _) = do
  ty <- M.newType
  void $ HLIR.MkTyPointer ty `M.isSubtypeOf` expected

  -- We check the pattern against the expected type, and then
  -- we wrap the typed pattern in a reference pattern.
  (typedPat, cs, bindings) <- checkP ty pat

  pure (HLIR.MkPatternReference typedPat (Identity expected), cs, bindings)

getAllFields :: (Monad m) => HLIR.Type -> m (Map Text HLIR.Type, HLIR.Type)
getAllFields (HLIR.MkTyRecord t) = getAllFields t
getAllFields (HLIR.MkTyRowExtend field fieldType _ rest) = do
  (fields, row) <- getAllFields rest
  pure (Map.insert field fieldType fields, row)
getAllFields t = pure (mempty, t)

removePatternLocation :: HLIR.HLIR "pattern" -> HLIR.HLIR "pattern"
removePatternLocation (HLIR.MkPatternLocated _ pat) = removePatternLocation pat
removePatternLocation pat = pat

-- | CHECK EXPRESSION
-- | Check an expression against an expected type.
-- | This function synthesizes the type of the expression, and then
-- | checks if the synthesized type is a subtype of the expected type.
-- | It returns the typed expression and any constraints generated during
-- | the synthesis.
checkE ::
  (MonadIO m, M.MonadError M.Error m) =>
  HLIR.Type ->
  HLIR.HLIR "expression" ->
  m (HLIR.TLIR "expression", M.Constraints, [(Text, HLIR.Scheme HLIR.Type)])
checkE (argTypes HLIR.:->: retType) (HLIR.MkExprLambda args retType' body) = do
  -- Checking that the number of parameters matches
  when (length argTypes /= length args) $
    M.throw (M.InvalidArgumentQuantity (length argTypes) (length args))

  -- Collecting old environment to restore it later
  oldEnv <- readIORef M.defaultCheckerState

  argsWithTypes <-
    zipWithM
      ( \p ty -> case p.typeValue of
          Just annotatedTy -> do
            aliasedTy <- M.performAliasRemoval annotatedTy

            void $ aliasedTy `M.isSubtypeOf` ty

            pure p {HLIR.typeValue = Identity aliasedTy}
          Nothing -> pure p {HLIR.typeValue = Identity ty}
      )
      args
      argTypes

  -- Adding parameters to the environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment =
          foldr
            ( \p (env :: [(Text, HLIR.Scheme HLIR.Type)]) ->
                (p.name, HLIR.Forall [] p.typeValue.runIdentity) : env
            )
            s.environment
            argsWithTypes,
        M.returnType = Just retType
      }

  forM_ retType' $ \t ->
    void $ t `M.isSubtypeOf` retType

  -- Checking the body against the return type
  (bodyExpr, cs, _) <- checkE retType body

  -- Restoring the old environment
  modifyIORef' M.defaultCheckerState $ \s ->
    s
      { M.environment = oldEnv.environment,
        M.returnType = oldEnv.returnType
      }

  -- Building parameter annotations with resolved types
  let paramAnnotations = zipWith (\p ty -> p {HLIR.typeValue = Identity ty}) args argTypes

  pure
    ( HLIR.MkExprLambda paramAnnotations (Identity retType) bodyExpr,
      cs,
      mempty
    )
checkE (HLIR.MkTyRecord expected) (HLIR.MkExprStructureCreation field value record _ _) = do
  (vTy, rTy, _) <- M.rewriteRow True False expected field

  (v', cs1, bs1) <- checkE vTy value
  (e', cs2, bs2) <- checkE (HLIR.MkTyRecord rTy) record

  pure (HLIR.MkExprStructureCreation field v' e' (Identity vTy) (Identity (HLIR.MkTyRecord expected)), cs1 <> cs2, bs1 <> bs2)
checkE expected HLIR.MkExprStructureEmpty = do
  let buildMap :: HLIR.Type -> Map Text HLIR.Type -> Map Text HLIR.Type
      buildMap (HLIR.MkTyRecord r) acc = buildMap r acc
      buildMap (HLIR.MkTyRowExtend field fieldType _ rest) acc =
        buildMap rest (Map.insert field fieldType acc)
      buildMap _ acc = acc

      isOption :: (MonadIO m) => HLIR.Type -> m Bool
      isOption (HLIR.MkTyApp (HLIR.MkTyId "Option") _) = pure True
      isOption (HLIR.MkTyVar v) = do
        tvr <- readIORef v
        case tvr of
          HLIR.Link ty -> isOption ty
          _ -> pure False
      isOption _ = pure False

  let expectedFields = buildMap expected mempty

  forM_ (Map.toList expectedFields) $ \(field, _) -> do
    (_, opt) <- fetchField expected field

    unless opt $
      M.throw (M.FieldNotFound field)

  reconstructedMap <-
    foldlM
      ( \acc (field, fieldType) -> do
          opt <- isOption fieldType
          if opt
            then do
              let noneE = HLIR.MkExprVariable (HLIR.MkAnnotation "None" (Identity fieldType)) []

              let recE = HLIR.MkExprStructureCreation field noneE acc (Identity fieldType) (Identity (HLIR.MkTyRecord expected))

              pure recE
            else M.throw (M.RewriteRowError expected field)
      )
      HLIR.MkExprStructureEmpty
      (Map.toList expectedFields)

  pure (reconstructedMap, mempty, mempty)
checkE expected (HLIR.MkExprLocated p e) = do
  HLIR.pushPosition p

  (typedExpr, cs, bindings) <- checkE expected e

  void HLIR.popPosition

  pure (HLIR.MkExprLocated p typedExpr, cs, bindings)
checkE expected (HLIR.MkExprVarCall f args) = do
  -- Synthesizing the type of the `self` expression
  (argsTys, argsExprs, cs1, bindings1) <- mapAndUnzip4M synthesizeE args

  -- Looking up the function in the environment
  state' <- readIORef M.defaultCheckerState
  let env = state'.environment

  pos <- HLIR.peekPosition'

  (funcType, _) <-
    findImplementationMatching env [] f (argsTys HLIR.:->: expected) pos

  case funcType of
    Just func -> do
      fRetType <- M.newType
      let funcType' = HLIR.MkTyFun argsTys fRetType

      void $ func `M.isSubtypeOf` funcType'
      void $ fRetType `M.isSubtypeOf` expected

      pure
        ( HLIR.MkExprApplication
            (HLIR.MkExprVariable (HLIR.MkAnnotation f (Identity func)) [])
            argsExprs
            (Identity fRetType),
          mconcat cs1 <> [M.MkImplConstraint f func pos []],
          mconcat bindings1
        )
    _ -> do
      properties <- readIORef M.defaultCheckerState <&> M.properties

      case Map.lookup f properties of
        Just scheme -> do
          -- If the function is a property, we treat it as a regular
          -- variable, but we also add a constraint that it must
          -- be implemented for the given types
          ty <- M.instantiateWithSub scheme [] >>= M.performAliasRemoval

          void $ ty `M.isSubtypeOf` (argsTys HLIR.:->: expected)

          let cs = concat cs1

          pure
            ( HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation f (Identity ty)) []) argsExprs (Identity expected),
              cs <> [M.MkImplConstraint f ty pos []],
              mconcat bindings1
            )
        Nothing -> M.throw (M.VariableNotFound f (Just (argsTys HLIR.:->: expected)))
checkE expected (HLIR.MkExprReturn expr) = do
  (typedExpr, cs, bindings) <- checkE expected expr

  pure (HLIR.MkExprReturn typedExpr, cs, bindings)
checkE expected expr = do
  (inferredTy, typedExpr, cs, bindings) <- synthesizeE expr

  void $ inferredTy `M.isSubtypeOf` expected

  pure (typedExpr, cs, bindings)

-- | Find a structure definition by its name.
-- | If the name is `Nothing`, we throw an `InvalidHeader` error.
-- | If the structure is not found, we throw a `StructureNotFound` error.
-- | If found, we return its type scheme.
findStructureMaybeById ::
  (MonadIO m, M.MonadError M.Error m) =>
  Maybe Text ->
  m (Maybe (HLIR.Scheme (Map Text HLIR.Type)))
findStructureMaybeById name = do
  structTypes <- readIORef M.defaultCheckerState <&> M.structures

  case name of
    Just n -> pure $ Map.lookup n structTypes
    Nothing -> pure Nothing

buildImplementations :: [(Text, HLIR.Scheme HLIR.Type)] -> [((Text, HLIR.Scheme HLIR.Type), HLIR.Scheme HLIR.Type)]
buildImplementations ((name, scheme) : xs) = case scheme of
  HLIR.Forall vars ((implTy : _) HLIR.:->: _) ->
    ((name, HLIR.Forall vars implTy), scheme) : buildImplementations xs
  _ -> buildImplementations xs
buildImplementations [] = []

-- | SOLVE CONSTRAINTS
-- | Solve a list of constraints by finding suitable implementations
-- | from the environment and ensuring that the implementation types
-- | are subtypes of the required types.
-- |
-- | This function modifies the type checker state by checking the
-- | implementations.
-- |
-- | If a constraint cannot be satisfied, it is ignored, as it may
-- | be handled later during code generation or runtime.
solveConstraints :: (MonadIO m, M.MonadError M.Error m) => M.Constraints -> m ()
solveConstraints constraints = do
  -- Getting the current implementations from the state
  environment <- readIORef M.defaultCheckerState <&> M.environment
  structures <- readIORef M.defaultCheckerState <&> Map.toList . M.structures

  -- For each constraint, we try to find a matching implementation
  -- If found, we affine the types by ensuring that the implementation
  -- type is a subtype of the required type.
  forM_ constraints $ \case
    M.MkImplConstraint name ty pos tys -> do
      mImplTy <- findImplementationMatching environment tys name ty pos
      case fst mImplTy of
        Just implTy -> void $ implTy `M.isSubtypeOf` ty
        Nothing -> pure () -- Ignoring unsolved constraints
    M.MkFieldConstraint structTy fieldName fieldTy pos -> do
      header <- getHeader <$> M.removeAliases structTy
      args <- getTypeArgs structTy

      case header of
        Just resolved ->
          findStructureMatching args structures resolved pos >>= \case
            Just structMap -> case Map.lookup fieldName structMap of
              Just expectedFieldTy -> void $ expectedFieldTy `M.isSubtypeOf` fieldTy
              Nothing -> M.throwError (M.FieldNotFound fieldName, pos)
            Nothing -> M.throwError (M.StructureNotFound structTy, pos)
        Nothing -> M.throwError (M.InvalidHeader, pos)
  where
    findStructureMatching ::
      (MonadIO m, M.MonadError M.Error m) =>
      [HLIR.Type] ->
      [(Text, HLIR.Scheme (Map Text HLIR.Type))] ->
      Text ->
      HLIR.Position ->
      m (Maybe (Map Text HLIR.Type))
    findStructureMatching _ [] _ _ = pure Nothing
    findStructureMatching args ((structName, scheme) : xs) name pos
      | structName == name = do
          -- We instantiate the structure scheme to get a concrete type
          structTy <- M.instantiateMapAndSub scheme args >>= mapM M.performAliasRemoval
          pure (Just structTy)
      | otherwise = findStructureMatching args xs name pos

findImplementationMatching ::
  (MonadIO m, M.MonadError M.Error m) =>
  [(Text, HLIR.Scheme HLIR.Type)] ->
  [HLIR.Type] ->
  Text ->
  HLIR.Type ->
  HLIR.Position ->
  m (Maybe HLIR.Type, M.Substitution)
findImplementationMatching [] _ _ _ _ = pure (Nothing, mempty)
findImplementationMatching ((implName, scheme) : xs) tys name ty pos
  | implName == name = do
      schemeTy' <- M.instantiateWithSub scheme tys >>= M.performAliasRemoval

      -- We check if the implementation type is a subtype of the required type
      -- If it is, we return it as a matching implementation.
      result <- runExceptT $ M.applySubtypeRelation False ty schemeTy'

      case result of
        Right s -> do
          instantiatedTy <- M.instantiateWithSub scheme tys >>= M.performAliasRemoval
          finalType <- M.applySubstitution s instantiatedTy

          pure (Just finalType, s)
        Left _ -> findImplementationMatching xs tys name ty pos
  | otherwise = findImplementationMatching xs tys name ty pos

-- | Extract the header (base type) from a type.
-- | For example, for `MyStruct[i32, bool]`, it returns `Just "MyStruct"`.
-- | For `MyStruct`, it returns `Just "MyStruct"`.
-- | For `i32`, it returns `Just "i32"`.
-- | For `i32[i32]`, it returns `Just "i32"`.
-- | For `i32 -> bool`, it returns `Nothing`.
getHeader :: HLIR.Type -> Maybe Text
getHeader (HLIR.MkTyApp (HLIR.MkTyId name) _) = Just name
getHeader (HLIR.MkTyId name) = Just name
getHeader _ = Nothing

-- | Extract type arguments from a type application.
-- | If the type is not a type application, return an empty list.
-- | This is used to get the applied types of a structure type.
-- | For example, for `MyStruct[i32, bool]`, it returns `[i32, bool]`.
-- | For `MyStruct`, it returns `[]`.
-- | For `i32`, it returns `[]`.
getTypeArgs :: (MonadIO m) => HLIR.Type -> m [HLIR.Type]
getTypeArgs (HLIR.MkTyApp _ args) = pure args
getTypeArgs (HLIR.MkTyVar ref) = do
  ty <- readIORef ref

  case ty of
    HLIR.Link t -> getTypeArgs t
    HLIR.Unbound _ _ -> pure []
getTypeArgs _ = pure []

-- | Find a property definition by its name.
-- | If the property is not found, we throw a `PropertyNotFound` error.
-- | If found, we return its type scheme.
findPropertyByName ::
  (MonadIO m, M.MonadError M.Error m) => Text -> m (HLIR.Scheme HLIR.Type)
findPropertyByName name = do
  -- Looking up the property in the environment
  properties <- readIORef M.defaultCheckerState <&> M.properties

  -- Throwing an error if the property is not found
  case Map.lookup name properties of
    Just scheme -> pure scheme
    Nothing -> M.newType >>= \ty -> pure (HLIR.Forall [] ty)

-- | Remove the third element from a triple.
-- | This is used to convert constraints from (name, type, position)
-- | to (name, type).
removeThird :: (a, b, c) -> (a, b)
removeThird (x, y, _) = (x, y)

-- | Check if the following type is a function type.
-- | If it is, return True, otherwise return False.
isFunctionType :: (MonadIO m) => HLIR.Type -> m Bool
isFunctionType (HLIR.MkTyFun _ _) = pure True
isFunctionType (HLIR.MkTyVar ref) = do
  ty <- readIORef ref

  case ty of
    HLIR.Link t -> isFunctionType t
    HLIR.Unbound _ _ -> pure False
isFunctionType _ = pure False

fetchField :: (MonadIO m) => HLIR.Type -> Text -> m (Maybe HLIR.Type, Bool)
fetchField (HLIR.MkTyRecord t) fieldName = fetchField t fieldName
fetchField (HLIR.MkTyRowExtend field fieldType isOpt rest) fieldName
  | field == fieldName = pure (Just fieldType, isOpt)
  | otherwise = fetchField rest fieldName
fetchField (HLIR.MkTyVar ref) fieldName = do
  ty <- readIORef ref
  case ty of
    HLIR.Link t -> fetchField t fieldName
    HLIR.Unbound _ _ -> pure (Nothing, False)
fetchField _ _ = pure (Nothing, False)

getVariable :: HLIR.HLIR "expression" -> Maybe Text
getVariable (HLIR.MkExprVariable (HLIR.MkAnnotation name _) _) = Just name
getVariable (HLIR.MkExprLocated _ e) = getVariable e
getVariable _ = Nothing
