module Language.Reality.Backend.Closure.Hoisting where

import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Syntax.HLIR qualified as HLIR

-- | HOISTING
-- | Hoist all let-in expressions to the top level of the program.
-- | This is done to simplify the closure conversion process.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with lambda expressions hoisted.
hoistLambdas ::
    (MonadIO m) =>
    [HLIR.TLIR "toplevel"] ->
    m [HLIR.TLIR "toplevel"]
hoistLambdas toplevels = do
    hoisted <- mapM hoistLambdaSingular toplevels
    pure (concat hoisted)

-- | Hoist a singular HLIR toplevel node.
-- | This function takes a toplevel node, and returns a list of toplevel
-- | nodes.
-- | This is used to hoist lambda expressions, as a lambda expression may
-- | resolve to multiple toplevel nodes.
hoistLambdaSingular ::
    (MonadIO m) =>
    HLIR.TLIR "toplevel" ->
    m [HLIR.TLIR "toplevel"]
hoistLambdaSingular (HLIR.MkTopLocated p e) = do
    hoisted <- hoistLambdaSingular e
    pure (map (HLIR.MkTopLocated p) hoisted)
hoistLambdaSingular (HLIR.MkTopFunctionDeclaration name params ret body) = do
    (newBody, hoisted) <- hoistLambdasInExpr body
    pure (hoisted ++ [HLIR.MkTopFunctionDeclaration name params ret newBody])
hoistLambdaSingular (HLIR.MkTopModuleDeclaration name body) = do
    hoistedBody <- hoistLambdas body
    pure [HLIR.MkTopModuleDeclaration name hoistedBody]
hoistLambdaSingular (HLIR.MkTopPublic node) = do
    hoisted <- hoistLambdaSingular node
    pure (map HLIR.MkTopPublic hoisted)
hoistLambdaSingular (HLIR.MkTopLet binding value) = do
    (newValue, hoistedValue) <- hoistLambdasInExpr value
    pure (hoistedValue ++ [HLIR.MkTopLet binding newValue])
hoistLambdaSingular node = pure [node]

-- | Hoist lambda expressions in a HLIR expression.
-- | This function takes an expression, and returns a tuple of the new expression
-- | with lambda expressions hoisted, and a list of toplevel nodes that were hoisted.
hoistLambdasInExpr ::
    (MonadIO m) =>
    HLIR.TLIR "expression" ->
    m (HLIR.TLIR "expression", [HLIR.TLIR "toplevel"])
hoistLambdasInExpr (HLIR.MkExprLocated p e) = do
    (newExpr, hoisted) <- hoistLambdasInExpr e
    pure (HLIR.MkExprLocated p newExpr, hoisted)
hoistLambdasInExpr (HLIR.MkExprApplication callee args retTy) = do
    (newCallee, hoistedCallee) <- hoistLambdasInExpr callee
    (newArgs, hoistedArgs) <- mapAndUnzipM hoistLambdasInExpr args
    pure
        ( HLIR.MkExprApplication newCallee newArgs retTy
        , hoistedCallee ++ concat hoistedArgs
        )
hoistLambdasInExpr (HLIR.MkExprVariable ann vars) =
    pure (HLIR.MkExprVariable ann vars, [])
hoistLambdasInExpr (HLIR.MkExprLiteral lit) =
    pure (HLIR.MkExprLiteral lit, [])
hoistLambdasInExpr (HLIR.MkExprLambda params ret body) = do
    (newBody, hoistedBody) <- hoistLambdasInExpr body

    -- Generate a unique name for the hoisted lambda function
    -- This is done to avoid name collisions with other functions.
    lambdaName <- freshLambdaName

    -- Building the function type for the hoisted lambda
    -- It takes the types of the parameters and the return type
    -- to construct the function type.
    --
    -- This is used to annotate the hoisted function.
    let funcType = HLIR.MkTyFun (map (runIdentity <$> (.typeValue)) params) ret.runIdentity

    -- Create the toplevel function declaration for the hoisted lambda
    let lambdaToplevel =
            HLIR.MkTopFunctionDeclaration
                (HLIR.MkAnnotation lambdaName [])
                (map (runIdentity <$>) params)
                ret.runIdentity
                newBody

    -- Return the new expression which is a variable referencing the hoisted lambda
    -- and the list of hoisted toplevel nodes which includes the new lambda function
    pure
        ( HLIR.MkExprVariable (HLIR.MkAnnotation lambdaName (Identity funcType)) []
        , hoistedBody ++ [lambdaToplevel]
        )
hoistLambdasInExpr (HLIR.MkExprLetIn binding value inExpr ret) = do
    (newValue, hoistedValue) <- hoistLambdasInExpr value
    (newInExpr, hoistedInExpr) <- hoistLambdasInExpr inExpr
    pure
        (HLIR.MkExprLetIn binding newValue newInExpr ret, hoistedValue ++ hoistedInExpr)
hoistLambdasInExpr (HLIR.MkExprCondition cond thenB elseB thenType elseType) = do
    (newCond, hoistedCond) <- hoistLambdasInExpr cond
    (newThenB, hoistedThenB) <- hoistLambdasInExpr thenB
    (newElseB, hoistedElseB) <- hoistLambdasInExpr elseB
    pure
        ( HLIR.MkExprCondition newCond newThenB newElseB thenType elseType
        , hoistedCond ++ hoistedThenB ++ hoistedElseB
        )
hoistLambdasInExpr (HLIR.MkExprStructureAccess struct field) = do
    (newStruct, hoistedStruct) <- hoistLambdasInExpr struct
    pure (HLIR.MkExprStructureAccess newStruct field, hoistedStruct)
hoistLambdasInExpr (HLIR.MkExprStructureCreation k e r ty rTy) = do
    (newE, hoistedE) <- hoistLambdasInExpr e
    (newR, hoistedR) <- hoistLambdasInExpr r

    pure (HLIR.MkExprStructureCreation k newE newR ty rTy, hoistedE ++ hoistedR)
hoistLambdasInExpr (HLIR.MkExprDereference e targetType) = do
    (newE, hoistedE) <- hoistLambdasInExpr e
    pure (HLIR.MkExprDereference newE targetType, hoistedE)
hoistLambdasInExpr (HLIR.MkExprReference e targetType) = do
    (newE, hoistedE) <- hoistLambdasInExpr e
    pure (HLIR.MkExprReference newE targetType, hoistedE)
hoistLambdasInExpr (HLIR.MkExprUpdate update value updateType) = do
    (newUpdate, hoistedUpdate) <- hoistLambdasInExpr update
    (newValue, hoistedValue) <- hoistLambdasInExpr value

    pure
        (HLIR.MkExprUpdate newUpdate newValue updateType, hoistedUpdate ++ hoistedValue)
hoistLambdasInExpr (HLIR.MkExprSizeOf t) =
    pure (HLIR.MkExprSizeOf t, [])
hoistLambdasInExpr (HLIR.MkExprSingleIf cond thenB branchType) = do
    (newCond, hoistedCond) <- hoistLambdasInExpr cond
    (newThenB, hoistedThenB) <- hoistLambdasInExpr thenB
    pure
        (HLIR.MkExprSingleIf newCond newThenB branchType, hoistedCond ++ hoistedThenB)
hoistLambdasInExpr (HLIR.MkExprCast e t) = do
    (newE, hoistedE) <- hoistLambdasInExpr e
    pure (HLIR.MkExprCast newE t, hoistedE)
hoistLambdasInExpr (HLIR.MkExprWhile cond body loopType inExpr) = do
    (newCond, hoistedCond) <- hoistLambdasInExpr cond
    (newBody, hoistedBody) <- hoistLambdasInExpr body
    (newInExpr, hoistedInExpr) <- hoistLambdasInExpr inExpr

    pure
        ( HLIR.MkExprWhile newCond newBody loopType newInExpr
        , hoistedCond ++ hoistedBody ++ hoistedInExpr
        )
hoistLambdasInExpr (HLIR.MkExprIs e p resultType) = do
    (newE, hoistedE) <- hoistLambdasInExpr e
    pure (HLIR.MkExprIs newE p resultType, hoistedE)
hoistLambdasInExpr (HLIR.MkExprReturn e) = do
    (newE, hoistedE) <- hoistLambdasInExpr e
    pure (HLIR.MkExprReturn newE, hoistedE)
hoistLambdasInExpr HLIR.MkExprBreak = pure (HLIR.MkExprBreak, [])
hoistLambdasInExpr HLIR.MkExprContinue = pure (HLIR.MkExprContinue, [])
hoistLambdasInExpr (HLIR.MkExprFunctionAccess ann this tys exprs) = do
    (newThis, hoistedThis) <- hoistLambdasInExpr this
    (newExprs, hoistedExprs) <- mapAndUnzipM hoistLambdasInExpr exprs
    pure
        ( HLIR.MkExprFunctionAccess ann newThis tys newExprs
        , hoistedThis ++ concat hoistedExprs
        )
hoistLambdasInExpr (HLIR.MkExprLetPatternIn pat value inExpr ret) = do
    (newValue, hoistedValue) <- hoistLambdasInExpr value
    (newInExpr, hoistedInExpr) <- hoistLambdasInExpr inExpr
    pure
        (HLIR.MkExprLetPatternIn pat newValue newInExpr ret, hoistedValue ++ hoistedInExpr)
hoistLambdasInExpr HLIR.MkExprStructureEmpty = pure (HLIR.MkExprStructureEmpty, [])

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO (newIORef 0)

-- | Generate a unique name for a hoisted lambda function.
-- | This is done by incrementing a counter stored in an IORef.
-- | The generated name is of the form "__lambda_<counter>".
freshLambdaName :: (MonadIO m) => m Text
freshLambdaName = do
    modifyIORef' symbolCounter (+ 1)
    c <- liftIO $ readIORef symbolCounter
    pure $ "__lambda_" <> Text.pack (show c)
