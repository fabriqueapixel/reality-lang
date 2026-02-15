module Language.Reality.Frontend.Module.Resolver where

import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Text qualified as Text
import Language.Reality.Syntax.HLIR qualified as HLIR
import qualified Data.Map as Map

-- | MODULE RESOLVER
-- | Resolve modules in a program.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with modules resolved.
runModuleResolver ::
    (MonadIO m, M.MonadError M.Error m) =>
    [HLIR.HLIR "toplevel"] ->
    m [HLIR.HLIR "toplevel"]
runModuleResolver toplevels = resolveModules toplevels []

-- | Resolve multiple nodes recursively
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with modules resolved.
-- | It takes a second argument which is the current path of modules being resolved.
resolveModules ::
    (MonadIO m, M.MonadError M.Error m) =>
    [HLIR.HLIR "toplevel"] ->
    Paths ->
    m [HLIR.HLIR "toplevel"]
resolveModules toplevels paths = do
    resolved <- mapM (`resolveModuleSingular` paths) toplevels
    pure (concat resolved)

-- | Resolve a singular HLIR toplevel node.
-- | This function takes a toplevel node, and returns a list of toplevel nodes.
-- | This is used to resolve modules, as a module may contain multiple toplevel
-- | nodes.
-- | It takes a second argument which is the current path of modules being resolved.
resolveModuleSingular ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.HLIR "toplevel" ->
    Paths ->
    m [HLIR.HLIR "toplevel"]
resolveModuleSingular (HLIR.MkTopLocated p e) paths = do
    resolved <- resolveModuleSingular e paths
    pure (map (HLIR.MkTopLocated p) resolved)
resolveModuleSingular (HLIR.MkTopModuleDeclaration name body) paths =
    resolveModules body (paths ++ [name])
resolveModuleSingular (HLIR.MkTopConstantDeclaration (HLIR.MkAnnotation name ty) expr) paths = do
    let newName = createName paths name
    pure [HLIR.MkTopConstantDeclaration (HLIR.MkAnnotation newName ty) expr]
resolveModuleSingular
    ( HLIR.MkTopFunctionDeclaration
            { HLIR.name = (HLIR.MkAnnotation name generics)
            , HLIR.parameters = params
            , HLIR.returnType = ret
            , HLIR.body = body
            }
        )
    paths = do
        let newName = createName paths name
        pure
            [ HLIR.MkTopFunctionDeclaration
                { HLIR.name = HLIR.MkAnnotation newName generics
                , HLIR.parameters = params
                , HLIR.returnType = ret
                , HLIR.body = body
                }
            ]
resolveModuleSingular
    ( HLIR.MkTopTypeAlias
            { HLIR.name = HLIR.MkAnnotation name generics
            , HLIR.boundType = typeValue
            }
        )
    paths = do
        let newName = createName paths name
        pure
            [ HLIR.MkTopTypeAlias
                { HLIR.name = HLIR.MkAnnotation newName generics
                , HLIR.boundType = typeValue
                }
            ]
resolveModuleSingular
    (HLIR.MkTopEnumeration
        { HLIR.name = HLIR.MkAnnotation name generics
        , HLIR.constructors = constructors
        }
    )
    paths = do
        let newName = createName paths name
        pure
            [ HLIR.MkTopEnumeration
                { HLIR.name = HLIR.MkAnnotation newName generics
                , HLIR.constructors = Map.mapKeys (createName paths) constructors
                }
            ]

resolveModuleSingular (HLIR.MkTopPublic node) paths = do
    resolved <- resolveModuleSingular node paths
    pure (map HLIR.MkTopPublic resolved)
resolveModuleSingular node _ = pure [node]

-- | Create a new name by combining the module paths with the given name.
-- | For example, if the paths are ["MyModule", "SubModule"] and the name is "MyType",
-- | the resulting name will be "MyModule::SubModule::MyType".
createName :: Paths -> Text -> Text
createName paths name = Text.intercalate "." (paths ++ [name])

-- | Type alias for module paths
-- | Paths represents the hierarchical path of modules,
-- | e.g. ["MyModule", "SubModule"]
type Paths = [Text]
