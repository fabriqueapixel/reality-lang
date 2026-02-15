module Language.Reality.Frontend.Typechecker.Unification where

import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Language.Reality.Frontend.Typechecker.Monad qualified as M
import Language.Reality.Syntax.HLIR qualified as HLIR
import Language.Reality.Frontend.Typechecker.Monad (Substitution)
import Control.Monad (foldM)

-- | SUBTYPING RELATION
-- | Check if a type is a subtype of another type.
-- | This is used to check if a type can be assigned to another type.
-- | For example, an Int can be assigned to a Float, but not vice versa.
-- | This function takes two types, and returns a boolean indicating if the first type
-- | is a subtype of the second type.
-- | It also returns a list of type substitutions that were made during the check.
-- | If the types are not compatible, it throws a TypeMismatch error.
isSubtypeOf ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.Type ->
    HLIR.Type ->
    m M.Substitution
isSubtypeOf t1 t2 = do
    aliasedT1 <- performAliasRemoval t1
    aliasedT2 <- performAliasRemoval t2

    applySubtypeRelation True aliasedT1 aliasedT2

-- | Simplify and remove type aliases from a type.
-- | This is used to prepare a type for unification or subtype checking.
-- | This function takes a type, and returns a simplified type with no aliases.
-- | If the type contains aliases that cannot be resolved, it throws an error.
performAliasRemoval ::
    (MonadIO m, M.MonadError M.Error m) => HLIR.Type -> m HLIR.Type
performAliasRemoval ty = do
    simplTy <- HLIR.simplify ty
    baseTy <- removeAliases simplTy

    HLIR.sanitizeRecord baseTy

removeAliases :: (MonadIO m, M.MonadError M.Error m) => HLIR.Type -> m HLIR.Type
removeAliases (HLIR.MkTyApp (HLIR.MkTyId base) args) = do
    typeAliases <- M.typeAliases <$> liftIO (readIORef M.defaultCheckerState)

    case Map.lookup base typeAliases of
        Just (HLIR.Forall qvars aliasedType) -> do
            when (length qvars /= length args)
                $ M.throw (M.InvalidArgumentQuantity (length qvars) (length args))

            let s = Map.fromList (zip qvars args)

            M.applySubstitution s aliasedType >>= removeAliases
        Nothing -> do
            newBase <- removeAliases (HLIR.MkTyId base)
            newArgs <- mapM removeAliases args
            pure (HLIR.MkTyApp newBase newArgs)
removeAliases (HLIR.MkTyId name) = do
    typeAliases <- M.typeAliases <$> liftIO (readIORef M.defaultCheckerState)

    case Map.lookup name typeAliases of
        Just (HLIR.Forall [] aliasedType) -> removeAliases aliasedType
        _ -> pure (HLIR.MkTyId name)
removeAliases (HLIR.MkTyVar ref) = do
    ty <- liftIO $ readIORef ref
    case ty of
        HLIR.Link ty' -> removeAliases ty'
        HLIR.Unbound{} -> pure (HLIR.MkTyVar ref)
removeAliases (HLIR.MkTyQuantified name) = pure (HLIR.MkTyQuantified name)
removeAliases (HLIR.MkTyRecord t) = do
    t' <- removeAliases t
    pure (HLIR.MkTyRecord t')
removeAliases HLIR.MkTyRowEmpty = pure HLIR.MkTyRowEmpty
removeAliases (HLIR.MkTyRowExtend label fieldType rest) = do
    fieldType' <- removeAliases fieldType
    rest' <- removeAliases rest
    pure (HLIR.MkTyRowExtend label fieldType' rest')
removeAliases (HLIR.MkTyApp base args) =
    HLIR.MkTyApp <$> removeAliases base <*> mapM removeAliases args

-- | Should the unification mutate the type variables
-- | - If True, the type variables will be mutated to point to the unified type.
-- | - If False, the type variables will not be mutated, and only a substitution
-- |   will be returned.
type ShouldMutate = Bool

-- | APPLY SUBTYPE RELATION
-- | Apply the subtype relation between two types.
-- | This function takes two types, and returns a substitution that makes the first
-- | type a subtype of the second type.
-- | If the types are not compatible, it throws a TypeMismatch error.
-- | The ShouldMutate parameter indicates whether the type variables should be mutated
-- | to point to the unified type.
-- |
-- | The rules for subtyping are as follows:
-- | - Function types are contravariant in their arguments and covariant in their return type :
-- |   A1 -> R1 <: A2 -> R2 if A2 <: A1 and R1 <: R2
-- |
-- | - Type variables can be unified with any type, as long as they do not create
-- |   a cyclic dependency.
-- |
-- | - Type applications are covariant in their arguments:
-- |   F[A1, A2, ..., An] <: F[B1, B2, ..., Bn] if Ai <: Bi for all i
-- |
-- | - Named types are equal if they have the same name, or if they are both integer
-- |   types with the same or compatible sizes (e.g., i32 <: i64).
-- |   Similarly for unsigned integer types (u8, u16, u32, u64, u128)
-- |   and floating-point types (f16, f32, f64, f128).
-- |   An integer type can be a subtype of an unsigned integer type if its size
-- |   is strictly less (e.g., i32 <: u64).
-- |
-- |   It should normally check by bound checking but for simplicity we just check sizes here.
-- |
-- | - All other types must be exactly equal to be considered subtypes.
applySubtypeRelation ::
    (MonadIO m, M.MonadError M.Error m) =>
    ShouldMutate ->
    HLIR.Type ->
    HLIR.Type ->
    m M.Substitution
applySubtypeRelation shouldMutate (argsF1 HLIR.:->: retF1) (argsF2 HLIR.:->: retF2)
    | length argsF1 == length argsF2 = do
        subsArgs <-
            zipWithM (flip (applySubtypeRelation shouldMutate)) argsF1 argsF2
        subRet <- applySubtypeRelation shouldMutate retF1 retF2

        composeSubstitutions (subsArgs ++ [subRet])
    | otherwise =
        M.throw (M.InvalidArgumentQuantity (length argsF1) (length argsF2))
applySubtypeRelation shouldMutate t1@(HLIR.MkTyVar ref1) t2 | t1 /= t2 = do
    ty1 <- readIORef ref1

    case ty1 of
        HLIR.Unbound name1 _ -> do
            occursCheck name1 t2

            when shouldMutate $ writeIORef ref1 (HLIR.Link t2)

            pure (Map.singleton name1 t2)
        HLIR.Link ty1' -> applySubtypeRelation shouldMutate ty1' t2
applySubtypeRelation shouldMutate t1 t2@(HLIR.MkTyVar ref2) | t1 /= t2 = do
    ty2 <- readIORef ref2

    case ty2 of
        HLIR.Unbound name2 _ -> do
            occursCheck name2 t1

            when shouldMutate $ writeIORef ref2 (HLIR.Link t1)

            pure (Map.singleton name2 t1)
        HLIR.Link ty2' -> applySubtypeRelation shouldMutate t1 ty2'
applySubtypeRelation shouldMutate (HLIR.MkTyApp base1 args1) (HLIR.MkTyApp base2 args2)
    | length args1 == length args2 = do
        sub <- applySubtypeRelation shouldMutate base1 base2
        subs <- composeSubstitutions =<< zipWithM (applySubtypeRelation shouldMutate) args1 args2
        composeSubstitution sub subs
    | otherwise = M.throw (M.InvalidArgumentQuantity (length args1) (length args2))
applySubtypeRelation _ _ (HLIR.MkTyId "never") = pure Map.empty
applySubtypeRelation _ (HLIR.MkTyId "never") _ = pure Map.empty
applySubtypeRelation _ (HLIR.MkTyId name1) (HLIR.MkTyId name2)
    | name1 == name2 = pure Map.empty
applySubtypeRelation _ (HLIR.MkTyQuantified name1) (HLIR.MkTyQuantified name2)
    | name1 == name2 = pure Map.empty
applySubtypeRelation _ HLIR.MkTyRowEmpty HLIR.MkTyRowEmpty = pure Map.empty
applySubtypeRelation shouldMutate (HLIR.MkTyRowExtend label1 fieldTy1 rowTail1) row2 = do
    (fieldTy2, rowTail2, s1) <- rewriteRow shouldMutate row2 label1
    -- ^ apply side-condition to ensure termination
    case snd $ toList' rowTail1 of
        Just tv -> do
            tvv <- readIORef tv
            case tvv of
                HLIR.Link ty -> applySubtypeRelation shouldMutate ty rowTail2
                HLIR.Unbound name _ -> do
                    occursCheck name fieldTy2

                    fieldTy1' <- M.applySubstitution s1 fieldTy1
                    fieldTy2' <- M.applySubstitution s1 fieldTy2
                    s2 <- applySubtypeRelation shouldMutate fieldTy1' fieldTy2'

                    sc <- composeSubstitution s2 s1

                    rowTail1' <- M.applySubstitution sc rowTail1
                    rowTail2' <- M.applySubstitution sc rowTail2
                    s3 <- applySubtypeRelation shouldMutate rowTail1' rowTail2'

                    composeSubstitutions [s3, sc]
        _ -> do
            fieldTy1' <- M.applySubstitution s1 fieldTy1
            fieldTy2' <- M.applySubstitution s1 fieldTy2
            s2 <- applySubtypeRelation shouldMutate fieldTy1' fieldTy2'

            sc <- composeSubstitution s2 s1

            rowTail1' <- M.applySubstitution sc rowTail1
            rowTail2' <- M.applySubstitution sc rowTail2
            s3 <- applySubtypeRelation shouldMutate rowTail1' rowTail2'

            composeSubstitutions [s3, sc]
applySubtypeRelation shouldMutate (HLIR.MkTyRecord t1) (HLIR.MkTyRecord t2) = applySubtypeRelation shouldMutate t1 t2
applySubtypeRelation _ t1 t2 | t1 /= t2 = M.throw (M.UnificationFail t1 t2)
applySubtypeRelation _ _ _ = pure Map.empty


toList' :: HLIR.Type -> ([(Text, HLIR.Type)], Maybe (IORef HLIR.TyVar))
toList' (HLIR.MkTyVar r) = ([], Just r)
toList' HLIR.MkTyRowEmpty = ([], Nothing)
toList' (HLIR.MkTyRowExtend l t r) =
  let (ls, mv) = toList' r
    in ((l, t):ls, mv)
toList' (HLIR.MkTyRecord r) = toList' r
toList' _ = M.compilerError "toList' called on non-row type"

-- | OCCURS CHECK
-- | Check if a type variable occurs in a type.
-- | This is used to prevent cyclic dependencies when unifying types.
-- | This function takes a type variable name and a type, and throws a CyclicTypeVariable
-- | error if the type variable occurs in the type.
-- | Otherwise, it does nothing.
occursCheck :: (MonadIO m, M.MonadError M.Error m) => Text -> HLIR.Type -> m ()
occursCheck name t@(HLIR.MkTyVar ref) = do
    ty <- readIORef ref
    case ty of
        HLIR.Link ty' -> occursCheck name ty'
        HLIR.Unbound name' _ ->
            when (name == name')
                $ M.throw (M.CyclicTypeVariable name t)
occursCheck name (HLIR.MkTyApp base args) = do
    occursCheck name base
    mapM_ (occursCheck name) args
occursCheck name (HLIR.MkTyRecord t) = occursCheck name t
occursCheck name (HLIR.MkTyRowExtend _ fieldTy rowTail) =
    occursCheck name fieldTy >> occursCheck name rowTail
occursCheck _ _ = pure ()

rewriteRow :: (MonadIO m, M.MonadError M.Error m) => Bool -> HLIR.Type -> Text -> m (HLIR.Type, HLIR.Type, Substitution)
rewriteRow _ HLIR.MkTyRowEmpty newLabel = M.throw $ M.RewriteRowError HLIR.MkTyRowEmpty newLabel
rewriteRow shouldMutate (HLIR.MkTyRowExtend label fieldTy rowTail) newLabel
    | newLabel == label = return (fieldTy, rowTail, mempty) -- ^ nothing to do
    | alpha@(HLIR.MkTyVar _) <- rowTail = do
        beta <- M.newType
        gamma <- M.newType

        s <- applySubtypeRelation shouldMutate alpha (HLIR.MkTyRowExtend newLabel gamma beta)

        return (gamma, HLIR.MkTyRowExtend label fieldTy beta, s)
    | otherwise = do
        (fieldTy', rowTail', s) <- rewriteRow shouldMutate rowTail newLabel

        return (fieldTy', HLIR.MkTyRowExtend label fieldTy rowTail', s)
rewriteRow _ ty label = M.throw $ M.RewriteRowError ty label

composeSubstitution :: MonadIO m => Substitution -> Substitution -> m Substitution
composeSubstitution s1 s2 = do
    s2' <- Map.traverseWithKey (\_ ty -> M.applySubstitution s1 ty) s2
    pure $ Map.union s1 s2'

composeSubstitutions :: MonadIO m => [Substitution] -> m Substitution
composeSubstitutions = foldM composeSubstitution Map.empty
