module Language.Reality.Frontend.Typechecker.Monad where

import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Syntax.HLIR qualified as HLIR
import Prelude hiding (Constraint)
import qualified Data.Set as Set

-- | The state of the type checker monad.
-- | This state includes:
-- |
-- | - A counter for generating fresh type variables.
-- | - The current type environment.
-- | - The current type aliases.
-- | - The current structures.
-- | - The current implementations.
-- | - The current properties.
-- |
-- | This state is used to keep track of the type information during type checking.
-- | It is stored in an IORef to allow for mutable state in the type checker monad.
data CheckerState = CheckerState
    { counter :: IORef Int
    , environment :: Map Text (HLIR.Scheme HLIR.Type)
    , typeAliases :: Map Text (HLIR.Scheme HLIR.Type)
    , structures :: Map Text (HLIR.Scheme (Map Text HLIR.Type))
    , implementations :: Map (Text, HLIR.Scheme HLIR.Type) (HLIR.Scheme HLIR.Type)
    , properties :: Map Text (HLIR.Scheme HLIR.Type)
    , returnType :: Maybe HLIR.Type
    , isInLoop :: Bool
    , typeVariables :: Set Text
    }
    deriving (Eq, Ord, Generic)

-- | SUBSTITUTION TYPE
-- | A substitution is a mapping from type variables to types. It is used to
-- | represent the result of unification or type inference.
type Substitution = Map Text HLIR.Type

-- | CONSTRAINTS TYPE
-- | A constraint is a tuple of a variable name, a type, and a position.
-- | It is used to represent a type constraint that needs to be satisfied.
-- | For instance a constraint can represent that a variable must have a certain type.
-- | This is mainly used during type inference to resolve implementation constraints.
type Constraints = [Constraint]

data Constraint
    = MkImplConstraint Text HLIR.Type HLIR.Position [HLIR.Type]
    | MkFieldConstraint HLIR.Type Text HLIR.Type HLIR.Position
    deriving (Eq, Ord, Generic)

-- | WITH ENVIRONMENT
-- | Temporarily extend the type environment with a new mapping.
-- | This is used to typecheck a function body with the function's parameters
-- | in scope.
-- | The environment is restored to its previous state after the action is completed.
-- | This function takes a mapping from variable names to type schemes, and an action
-- | to perform with the extended environment.
withEnvironment :: (MonadIO m) => Map Text (HLIR.Scheme HLIR.Type) -> m a -> m a
withEnvironment env action = do
    ref <- liftIO $ readIORef defaultCheckerState
    let oldEnv = ref.environment
    liftIO $ writeIORef defaultCheckerState ref{environment = Map.union env oldEnv}
    result <- action
    ref' <- liftIO $ readIORef defaultCheckerState
    liftIO $ writeIORef defaultCheckerState ref'{environment = oldEnv}
    pure result

defaultCheckerState :: IORef CheckerState
defaultCheckerState = IO.unsafePerformIO $ do
    ref <- newIORef 0
    newIORef
        CheckerState
            { counter = ref
            , environment = Map.empty
            , typeAliases = Map.empty
            , structures = Map.empty
            , implementations = Map.empty
            , properties = Map.empty
            , returnType = Nothing
            , isInLoop = False
            , typeVariables = Set.empty
            }

-- | NEW SYMBOL
-- | Generate a new unique symbol with the given prefix.
-- | This is used to generate fresh type variables during type inference.
-- | The generated symbol is guaranteed to be unique within the current type checker
-- | state.
newSymbol :: (MonadIO m) => Text -> m Text
newSymbol prefix = do
    ref <- liftIO $ readIORef defaultCheckerState
    let c = ref.counter

    liftIO $ modifyIORef c (+ 1)
    i <- liftIO $ readIORef c

    pure $ prefix <> Text.pack (show i)

-- | NEW TYPE
-- | Generate a new fresh type variable.
-- | The generated type variable is guaranteed to be unique within the current type
-- | checker state.
-- | This function uses `newSymbol` to generate a unique name for the type variable,
-- | and then creates a new `IORef` to hold the type variable's state.
newType :: (MonadIO m) => m HLIR.Type
newType = do
    sym <- newSymbol "t"
    ioref <- newIORef (HLIR.Unbound sym 0)
    pure $ HLIR.MkTyVar ioref

-- | RESET STATE
-- | Reset the type checker state to its initial state.
-- | This is used to clear the type checker state between different type checking
-- | runs, if needed.
resetState :: (MonadIO m) => m ()
resetState = do
    liftIO
        $ writeIORef
            defaultCheckerState
            CheckerState
                { counter = IO.unsafePerformIO (newIORef 0)
                , environment = Map.empty
                , typeAliases = Map.empty
                , structures = Map.empty
                , implementations = Map.empty
                , properties = Map.empty
                , returnType = Nothing
                , isInLoop = False
                , typeVariables = Set.empty
                }

-- | INSTANTIATE AND SUB
-- | Instantiate a type scheme by replacing its quantified variables with fresh type
-- | variables.
-- | It also returns the substitution used for the instantiation.
instantiateAndSub ::
    (MonadIO m) => HLIR.Scheme HLIR.Type -> m (HLIR.Type, Map Text HLIR.Type)
instantiateAndSub (HLIR.Forall qvars schemeTy) = do
    newVars <- forM qvars $ const newType
    let s = Map.fromList (zip qvars newVars)

    ty <- applySubstitution s schemeTy

    pure (ty, s)

-- | INSTANTIATE MAP AND SUB
-- | Instantiate a type scheme that contains a map of types by replacing its quantified
-- | variables with fresh type variables.
-- | It also returns the substitution used for the instantiation.
instantiateMapWithSub ::
    (MonadIO m) =>
    HLIR.Scheme (Map Text HLIR.Type) -> m (Map Text HLIR.Type, Map Text HLIR.Type)
instantiateMapWithSub (HLIR.Forall qvars schemeTy) = do
    newVars <- forM qvars $ const newType
    let s = Map.fromList (zip qvars newVars)
    ty <- Map.traverseWithKey (\_ ty -> applySubstitution s ty) schemeTy

    pure (ty, s)

instantiateMapAndSub ::
    (MonadIO m) => HLIR.Scheme (Map Text HLIR.Type) ->
    [HLIR.Type] ->
    m (Map Text HLIR.Type)
instantiateMapAndSub (HLIR.Forall qvars schemeTy) types = do
    let subst = zip qvars types
        rest = drop (length types) qvars
    newVars <- forM rest $ const newType
    let s' = Map.fromList (subst ++ zip rest newVars)
    Map.traverseWithKey (\_ ty -> applySubstitution s' ty) schemeTy

-- | INSTANTIATE WITH SUB
-- | Instantiate a type scheme by replacing its quantified variables with the given
-- | types.
instantiateWithSub ::
    (MonadIO m) => HLIR.Scheme HLIR.Type -> [HLIR.Type] -> m HLIR.Type
instantiateWithSub (HLIR.Forall qvars schemeTy) types = do
    let subst = zip qvars types
        rest = drop (length types) qvars
    newVars <- forM rest $ const newType
    let s' = Map.fromList (subst ++ zip rest newVars)
    applySubstitution s' schemeTy

-- | INSTANTIATE
-- | Instantiate a type scheme by replacing its quantified variables with fresh type
-- | variables.
instantiate :: (MonadIO m) => HLIR.Scheme HLIR.Type -> m HLIR.Type
instantiate (HLIR.Forall qvars schemeTy) = fst <$> instantiateAndSub (HLIR.Forall qvars schemeTy)

-- | INSTANTIATE MAP
-- | Instantiate a type scheme that contains a map of types by replacing its quantified
-- | variables with fresh type variables.
instantiateMap ::
    (MonadIO m) => HLIR.Scheme (Map Text HLIR.Type) -> m (Map Text HLIR.Type)
instantiateMap (HLIR.Forall qvars schemeTy) = fst <$> instantiateMapWithSub (HLIR.Forall qvars schemeTy)

-- | APPLY SUBSTITUTION
-- | Apply a substitution to a type, replacing all occurrences of the type variables
-- | in the substitution with their corresponding types.
-- | This function recursively traverses the type and applies the substitution to
-- | all type variables and type applications.
applySubstitution ::
    (MonadIO m) => Map Text HLIR.Type -> HLIR.Type -> m HLIR.Type
applySubstitution s (HLIR.MkTyVar tvr) = do
    tv <- liftIO $ readIORef tvr
    case tv of
        HLIR.Link ty -> applySubstitution s ty
        HLIR.Unbound n _ -> pure $ case Map.lookup n s of
            Just ty -> ty
            Nothing -> HLIR.MkTyVar tvr
applySubstitution s (HLIR.MkTyApp t ts) = do
    t' <- applySubstitution s t
    ts' <- mapM (applySubstitution s) ts
    pure $ HLIR.MkTyApp t' ts'
applySubstitution s (HLIR.MkTyId name) =
    case Map.lookup name s of
        Just ty -> pure ty
        Nothing -> pure $ HLIR.MkTyId name
applySubstitution s (HLIR.MkTyQuantified name) =
    case Map.lookup name s of
        Just ty -> pure ty
        Nothing -> pure $ HLIR.MkTyQuantified name
applySubstitution s (HLIR.MkTyRecord t) = do
    t' <- applySubstitution s t
    pure $ HLIR.MkTyRecord t'   
applySubstitution _ HLIR.MkTyRowEmpty = pure HLIR.MkTyRowEmpty
applySubstitution s (HLIR.MkTyRowExtend label fieldType rest) = do
    fieldType' <- applySubstitution s fieldType
    rest' <- applySubstitution s rest
    pure $ HLIR.MkTyRowExtend label fieldType' rest'
