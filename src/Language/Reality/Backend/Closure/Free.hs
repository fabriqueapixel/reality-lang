module Language.Reality.Backend.Closure.Free where

import Data.Map qualified as Map
import Language.Reality.Syntax.HLIR qualified as HLIR

class Free a where
    free :: a -> Map Text HLIR.Type

instance (Free a) => Free [a] where
    free = foldMap free

instance (Free a) => Free (Maybe a) where
    free = foldMap free

instance (Free a, Free b) => Free (a, b) where
    free (a, b) = free a <> free b

instance (Free a, Free b, Free c) => Free (a, b, c) where
    free (a, b, c) = free a <> free b <> free c

instance Free (HLIR.Expression Identity HLIR.Type) where
    free e = do
        let (freeVars, freePat) = removePatternFromFree e
        freeVars Map.\\ freePat

removePatternFromFree ::
    HLIR.TLIR "expression" ->
    (Map Text HLIR.Type, Map Text HLIR.Type)
removePatternFromFree (HLIR.MkExprApplication f args _) = do
    let (freeF, freeP) = removePatternFromFree f
        (freeArgs, freeArgsPat) =
            foldl'
                (\(accFree, accPat) arg -> do
                    let (freeArg, freePattern) = removePatternFromFree arg
                        freeArg' = freeArg Map.\\ accPat
                    (accFree <> freeArg', accPat <> freePattern)
                )
                (mempty, mempty)
                args
    (freeF <> freeArgs, freeP <> freeArgsPat)
removePatternFromFree (HLIR.MkExprLambda params _ body) = do
    let (freeBody, freeP) = removePatternFromFree body
        freeP' = foldl' (\acc param -> Map.delete param.name acc) freeP params
    (freeBody Map.\\ Map.fromList (map HLIR.unannotate params), freeP')
removePatternFromFree (HLIR.MkExprLetIn binding value inExpr _) = do
    let (freeValue, freeP1) = removePatternFromFree value
        (freeInExpr, freeP2) = removePatternFromFree inExpr
        freeP' = freeP1 <> freeP2
        freeP'' = Map.delete binding.name freeP'
    (   (freeValue <> freeInExpr)
        Map.\\ Map.singleton binding.name binding.typeValue.runIdentity
        , freeP''
        )
removePatternFromFree (HLIR.MkExprCondition cond thenB elseB _ _) = do
    let (freeCond, freeP1) = removePatternFromFree cond
        (freeThenB, freeP2) = removePatternFromFree thenB
        (freeElseB, freeP3) = removePatternFromFree elseB
        freeP' = freeP1 <> freeP2 <> freeP3
    (   (freeCond <> freeThenB <> freeElseB)
        Map.\\ freeP1
        , freeP'
        )
removePatternFromFree (HLIR.MkExprSingleIf cond thenB _) = do
    let (freeCond, freeP1) = removePatternFromFree cond
        (freeThenB, freeP2) = removePatternFromFree thenB
        freeP' = freeP1 <> freeP2
    (   (freeCond <> freeThenB)
        Map.\\ freeP1
        , freeP'
        )
removePatternFromFree (HLIR.MkExprIs e p _) = do
    let (freeE, freeP) = removePatternFromFree e
        freeP' = free p
    (freeE Map.\\ freeP', freeP <> freeP')
removePatternFromFree (HLIR.MkExprLocated _ e) = removePatternFromFree e
removePatternFromFree (HLIR.MkExprStructureAccess struct _) = removePatternFromFree struct
removePatternFromFree (HLIR.MkExprStructureCreation _ e r _ _) = do
    let (freeE, freeP1) = removePatternFromFree e
        (freeR, freeP2) = removePatternFromFree r
    (freeE <> freeR, freeP1 <> freeP2)
removePatternFromFree (HLIR.MkExprDereference e _) = removePatternFromFree e
removePatternFromFree (HLIR.MkExprReference e _) = removePatternFromFree e
removePatternFromFree (HLIR.MkExprUpdate updatable expr _) = do
    let (freeUpdatable, freeP1) = removePatternFromFree updatable
        (freeExpr, freeP2) = removePatternFromFree expr
    (freeUpdatable <> freeExpr, freeP1 <> freeP2)
removePatternFromFree (HLIR.MkExprWhile cond body _ inExpr) = do
    let (freeCond, freeP1) = removePatternFromFree cond
        (freeBody, freeP2) = removePatternFromFree body
        (freeInExpr, freeP3) = removePatternFromFree inExpr
    (   freeCond <> freeBody <> freeInExpr
        , freeP1 <> freeP2 <> freeP3
        )
removePatternFromFree (HLIR.MkExprFunctionAccess _ this _ exprs) = do
    let (freeThis, freeP1) = removePatternFromFree this
        (freeExprs, freeP2) = foldl' (\(accFree, accP) expr -> do
                let (freeExpr, freePattern) = removePatternFromFree expr
                (accFree <> freeExpr, accP <> freePattern)
            ) (Map.empty, Map.empty) exprs
    (freeThis <> freeExprs, freeP1 <> freeP2)
removePatternFromFree (HLIR.MkExprReturn e) = removePatternFromFree e
removePatternFromFree HLIR.MkExprBreak = (Map.empty, Map.empty)
removePatternFromFree HLIR.MkExprContinue = (Map.empty, Map.empty)
removePatternFromFree (HLIR.MkExprVariable ann _) = (Map.singleton ann.name ann.typeValue.runIdentity, Map.empty)
removePatternFromFree (HLIR.MkExprLiteral _) = (Map.empty, Map.empty)
removePatternFromFree (HLIR.MkExprCast e _) = removePatternFromFree e
removePatternFromFree (HLIR.MkExprSizeOf _) = (Map.empty, Map.empty)
removePatternFromFree (HLIR.MkExprLetPatternIn pat value inExpr _) = do
    let (freeValue, freeP1) = removePatternFromFree value
        (freeInExpr, freeP2) = removePatternFromFree inExpr
        freeP' = freeP1 <> freeP2 <> free pat
    (   (freeValue <> freeInExpr)
        Map.\\ free pat
        , freeP'
        )
removePatternFromFree HLIR.MkExprStructureEmpty = (Map.empty, Map.empty)

instance Free (HLIR.Pattern Identity HLIR.Type) where
    free :: HLIR.Pattern Identity HLIR.Type -> Map Text HLIR.Type
    free (HLIR.MkPatternVariable _) = mempty
    free (HLIR.MkPatternLet x) = Map.singleton x.name x.typeValue.runIdentity
    free (HLIR.MkPatternConstructor _ patterns _) = free patterns
    free (HLIR.MkPatternLiteral _) = Map.empty
    free (HLIR.MkPatternStructure fields) = free (Map.elems fields)
    free HLIR.MkPatternWildcard = Map.empty
    free (HLIR.MkPatternLocated _ p) = free p
