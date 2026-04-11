{-# LANGUAGE LambdaCase #-}

module Language.Reality.Frontend.Module.Resolver where

import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.C
import Language.C.Data.Ident (Ident (Ident))
import Language.C.System.GCC (newGCC)
import Language.Reality.Syntax.HLIR qualified as HLIR
import System.FilePath ((</>))

{-# NOINLINE cwdGlobal #-}
cwdGlobal :: IORef FilePath
cwdGlobal = IO.unsafePerformIO (newIORef "")

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

getAnnotations :: HLIR.HLIR "toplevel" -> ([HLIR.HLIR "expression"], HLIR.HLIR "toplevel")
getAnnotations (HLIR.MkTopAnnotation anns node) = do
  let (anns', node') = getAnnotations node
  (anns ++ anns', node')
getAnnotations (HLIR.MkTopLocated _ node) = getAnnotations node
getAnnotations node = ([], node)

fetchAnnotation :: Text -> [HLIR.HLIR "expression"] -> Maybe [HLIR.HLIR "expression"]
fetchAnnotation name (ann : anns) = do
  case removeLocations ann of
    HLIR.MkExprApplication fun args _ -> do
      case removeLocations fun of
        HLIR.MkExprVariable annName _ | annName.name == name -> do
          Just args
        _ -> fetchAnnotation name anns
    _ -> fetchAnnotation name anns
fetchAnnotation _ [] = Nothing

removeLocations :: HLIR.HLIR "expression" -> HLIR.HLIR "expression"
removeLocations (HLIR.MkExprLocated _ e) = removeLocations e
removeLocations e = e

getStringLiteral :: HLIR.HLIR "expression" -> Maybe Text
getStringLiteral (HLIR.MkExprApplication fun args _)
  | HLIR.MkExprVariable annName _ <- fun,
    annName.name == "String.init" =
      case map removeLocations args of
        (HLIR.MkExprString s : _) -> Just s
        _ -> Nothing
getStringLiteral (HLIR.MkExprString s) = Just s
getStringLiteral (HLIR.MkExprLocated _ e) = getStringLiteral e
getStringLiteral _ = Nothing

resolveHeaderFile :: (MonadIO m, M.MonadError M.Error m) => FilePath -> m ([HLIR.HLIR "toplevel"], [HLIR.HLIR "toplevel"])
resolveHeaderFile path = do
  cwd <- liftIO $ readIORef cwdGlobal
  let fullPath = cwd </> path

  c <- liftIO (parseCFile (newGCC "gcc") Nothing [] fullPath)

  case c of
    Left err -> M.throw (M.CompilerError (Text.pack ("Failed to parse header file: " <> show err)))
    Right ast -> interpretHeaderFile ast

interpretHeaderFile :: (MonadIO m, M.MonadError M.Error m) => CTranslUnit -> m ([HLIR.HLIR "toplevel"], [HLIR.HLIR "toplevel"])
interpretHeaderFile (CTranslUnit decls _) = do
  mconcat <$> mapM interpretHeaderDecl decls

interpretHeaderDecl :: (MonadIO m, M.MonadError M.Error m, Show a) => CExternalDeclaration a -> m ([HLIR.HLIR "toplevel"], [HLIR.HLIR "toplevel"])
interpretHeaderDecl = \case
  CDeclExt decl -> interpretHeaderDecl' decl
  CFDefExt _ -> pure ([], [])
  CAsmExt _ _ -> pure ([], [])

interpretHeaderDecl' :: (MonadIO m, M.MonadError M.Error m, Show a) => CDeclaration a -> m ([HLIR.HLIR "toplevel"], [HLIR.HLIR "toplevel"])
interpretHeaderDecl' (CDecl specs decls _) = do
  let isTypedef = any isTypedefStorage specs
  baseType <- cDeclSpecsToType specs
  let remappedStructs = remapStandaloneStruct specs

  pure (remappedStructs, mapMaybe (declToToplevel isTypedef baseType) decls)
interpretHeaderDecl' (CStaticAssert {}) = pure ([], [])

isTypedefStorage :: CDeclarationSpecifier a -> Bool
isTypedefStorage = \case
  CStorageSpec (CTypedef _) -> True
  _ -> False

cDeclSpecsToType :: (MonadIO m, M.MonadError M.Error m) => [CDeclarationSpecifier a] -> m HLIR.Type
cDeclSpecsToType specs =
  case mapMaybe getTypeSpec specs of
    [] -> pure HLIR.MkTyInt
    (tySpec : _) -> cTypeSpecToType tySpec
  where
    getTypeSpec = \case
      CTypeSpec tySpec -> Just tySpec
      _ -> Nothing

cTypeSpecToType :: (MonadIO m, M.MonadError M.Error m) => CTypeSpecifier a -> m HLIR.Type
cTypeSpecToType = \case
  CVoidType _ -> pure HLIR.MkTyUnit
  CCharType _ -> pure HLIR.MkTyChar
  CIntType _ -> pure HLIR.MkTyInt
  CFloatType _ -> pure HLIR.MkTyFloat
  CDoubleType _ -> pure HLIR.MkTyFloat
  CBoolType _ -> pure HLIR.MkTyBool
  CSUType comp _ -> pure (compositeToType comp)
  CEnumType _ _ -> pure HLIR.MkTyInt
  CTypeDef ident _ -> pure (HLIR.MkTyId (identToText ident))
  CLongType _ -> pure HLIR.MkTyInt
  CShortType _ -> pure HLIR.MkTyInt
  CSignedType _ -> pure HLIR.MkTyInt
  CUnsigType _ -> pure HLIR.MkTyInt
  _ -> pure HLIR.MkTyInt

compositeToTyId :: CStructureUnion a -> HLIR.Type
compositeToTyId (CStruct _ maybeName _ _ _) =
  case maybeName of
    Just ident -> HLIR.MkTyId (identToText ident)
    Nothing -> HLIR.MkTyId "anonymous_struct"

compositeToType :: CStructureUnion a -> HLIR.Type
compositeToType su@(CStruct tag _ maybeDecls _ _)
  | tag == CStructTag,
    Just decls <- maybeDecls,
    not (null decls) =
      structDeclsToRecordType decls
  | otherwise = compositeToTyId su

remapStandaloneStruct :: [CDeclarationSpecifier a] -> [HLIR.HLIR "toplevel"]
remapStandaloneStruct specs =
  case mapMaybe getStructTypeSpec specs of
    [] -> []
    (CStruct tag maybeName maybeDecls _ _ : _)
      | tag == CStructTag,
        Just ident <- maybeName,
        Just decls <- maybeDecls ->
          [ HLIR.MkTopTypeAlias
              { HLIR.name = HLIR.MkAnnotation (identToText ident) [],
                HLIR.boundType = structDeclsToRecordType decls
              }
          ]
    _ -> []
  where
    getStructTypeSpec = \case
      CTypeSpec (CSUType su _) -> Just su
      _ -> Nothing

structDeclsToRecordType :: [CDeclaration a] -> HLIR.Type
structDeclsToRecordType decls =
  HLIR.MkTyRecord (foldr rowStep HLIR.MkTyRowEmpty fields)
  where
    fields = concatMap structMemberFields decls
    rowStep (HLIR.MkAnnotation fieldName fieldType) = HLIR.MkTyRowExtend fieldName fieldType False

structMemberFields :: CDeclaration a -> [HLIR.Annotation HLIR.Type]
structMemberFields (CDecl specs decls _) =
  let baseTy =
        case mapMaybe specType specs of
          [] -> HLIR.MkTyInt
          (tySpec : _) -> fromMaybe HLIR.MkTyInt (cTypeSpecToTypePure tySpec)
   in mapMaybe (memberDeclaratorToAnnotation baseTy) decls
structMemberFields (CStaticAssert {}) = []

memberDeclaratorToAnnotation :: HLIR.Type -> (Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a)) -> Maybe (HLIR.Annotation HLIR.Type)
memberDeclaratorToAnnotation baseTy (maybeDeclr, _, _) = do
  declr <- maybeDeclr
  info <- extractDeclInfo baseTy declr
  case info of
    ValueDecl name ty -> Just (HLIR.MkAnnotation name ty)
    FunctionDecl name params ret ->
      let argTypes = map HLIR.typeValue params
       in Just (HLIR.MkAnnotation name (HLIR.MkTyFun argTypes ret))

declToToplevel :: Bool -> HLIR.Type -> (Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a)) -> Maybe (HLIR.HLIR "toplevel")
declToToplevel isTypedef baseType (maybeDeclr, _, _) = do
  declr <- maybeDeclr
  declInfo <- extractDeclInfo baseType declr
  pure (declInfoToNode isTypedef declInfo)

data DeclInfo
  = ValueDecl Text HLIR.Type
  | FunctionDecl Text [HLIR.Annotation HLIR.Type] HLIR.Type

extractDeclInfo :: HLIR.Type -> CDeclarator a -> Maybe DeclInfo
extractDeclInfo baseType (CDeclr maybeIdent derived _ _ _) = do
  ident <- maybeIdent
  let (isFunction, paramTypes, ty) = foldl' applyDerived (False, [], baseType) derived
      name = identToText ident

  if isFunction
    then Just (FunctionDecl name paramTypes ty)
    else Just (ValueDecl name ty)

applyDerived :: (Bool, [HLIR.Annotation HLIR.Type], HLIR.Type) -> CDerivedDeclarator a -> (Bool, [HLIR.Annotation HLIR.Type], HLIR.Type)
applyDerived (isFun, params, currentType) = \case
  CPtrDeclr _ _ -> (isFun, params, HLIR.MkTyPointer currentType)
  CArrDeclr {} -> (isFun, params, HLIR.MkTyPointer currentType)
  CFunDeclr paramDecls _ _ ->
    let parsedParams = parseFunctionParams paramDecls
     in (True, parsedParams, currentType)

parseFunctionParams :: Either [Ident] ([CDeclaration a], Bool) -> [HLIR.Annotation HLIR.Type]
parseFunctionParams = \case
  Left _ -> []
  Right (decls, _) ->
    mapMaybe parseOneParam decls

parseOneParam :: CDeclaration a -> Maybe (HLIR.Annotation HLIR.Type)
parseOneParam (CDecl specs decls _) = do
  (maybeDeclr, _, _) <- firstDecl decls
  declr <- maybeDeclr
  let baseTy =
        case mapMaybe specType specs of
          [] -> HLIR.MkTyInt
          (tySpec : _) -> fromMaybe HLIR.MkTyInt (cTypeSpecToTypePure tySpec)
  paramInfo <- extractDeclInfo baseTy declr
  case paramInfo of
    ValueDecl name ty -> Just (HLIR.MkAnnotation name ty)
    FunctionDecl name params ret ->
      let argTypes = map HLIR.typeValue params
       in Just (HLIR.MkAnnotation name (HLIR.MkTyFun argTypes ret))
parseOneParam _ = Nothing

specType :: CDeclarationSpecifier a -> Maybe (CTypeSpecifier a)
specType = \case
  CTypeSpec t -> Just t
  _ -> Nothing

cTypeSpecToTypePure :: CTypeSpecifier a -> Maybe HLIR.Type
cTypeSpecToTypePure = \case
  CVoidType _ -> Just (HLIR.MkTyId "never")
  CCharType _ -> Just HLIR.MkTyChar
  CIntType _ -> Just HLIR.MkTyInt
  CFloatType _ -> Just HLIR.MkTyFloat
  CDoubleType _ -> Just HLIR.MkTyFloat
  CBoolType _ -> Just HLIR.MkTyBool
  CEnumType _ _ -> Just HLIR.MkTyInt
  CTypeDef ident _ -> Just (HLIR.MkTyId (identToText ident))
  CSUType comp _ -> Just (compositeToType comp)
  CLongType _ -> Just HLIR.MkTyInt
  CShortType _ -> Just HLIR.MkTyInt
  CSignedType _ -> Just HLIR.MkTyInt
  CUnsigType _ -> Just HLIR.MkTyInt
  _ -> Nothing

firstDecl :: [a] -> Maybe a
firstDecl [] = Nothing
firstDecl (x : _) = Just x

declInfoToNode :: Bool -> DeclInfo -> HLIR.HLIR "toplevel"
declInfoToNode isTypedef = \case
  ValueDecl name ty
    | isTypedef ->
        HLIR.MkTopTypeAlias
          { HLIR.name = HLIR.MkAnnotation name [],
            HLIR.boundType = ty
          }
    | otherwise -> HLIR.MkTopExternLet (HLIR.MkAnnotation name ty)
  FunctionDecl name params ret ->
    HLIR.MkTopExternalFunction
      { HLIR.name = HLIR.MkAnnotation name [],
        HLIR.parameters = params,
        HLIR.returnType = ret
      }

identToText :: Ident -> Text
identToText (Ident raw _ _) = Text.pack raw

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
  HLIR.pushPosition p
  resolved <- resolveModuleSingular e paths
  void HLIR.popPosition

  pure (map (HLIR.MkTopLocated p) resolved)
resolveModuleSingular n paths
  | (anns, node) <- getAnnotations n,
    not (null anns),
    HLIR.MkTopModuleDeclaration name [] <- node,
    Just ann <- fetchAnnotation "include" anns =
      do
        let paths' = map removeLocations ann
        let strPaths = mapMaybe getStringLiteral paths'

        (structs, headers) <- mconcat <$> mapM (resolveHeaderFile . Text.unpack) strPaths

        newHeaders <- resolveModules headers (paths ++ [name])

        pure (structs ++ newHeaders)
resolveModuleSingular (HLIR.MkTopModuleDeclaration name body) paths =
  resolveModules body (paths ++ [name])
resolveModuleSingular (HLIR.MkTopConstantDeclaration (HLIR.MkAnnotation name ty) expr) paths = do
  let newName = createName paths name
  pure [HLIR.MkTopConstantDeclaration (HLIR.MkAnnotation newName ty) expr]
resolveModuleSingular
  ( HLIR.MkTopFunctionDeclaration
      { HLIR.name = (HLIR.MkAnnotation name generics),
        HLIR.parameters = params,
        HLIR.returnType = ret,
        HLIR.body = body
      }
    )
  paths = do
    let newName = createName paths name
    pure
      [ HLIR.MkTopFunctionDeclaration
          { HLIR.name = HLIR.MkAnnotation newName generics,
            HLIR.parameters = params,
            HLIR.returnType = ret,
            HLIR.body = body
          }
      ]
resolveModuleSingular
  ( HLIR.MkTopTypeAlias
      { HLIR.name = HLIR.MkAnnotation name generics,
        HLIR.boundType = typeValue
      }
    )
  paths = do
    let newName = createName paths name
    pure
      [ HLIR.MkTopTypeAlias
          { HLIR.name = HLIR.MkAnnotation newName generics,
            HLIR.boundType = typeValue
          }
      ]
resolveModuleSingular
  ( HLIR.MkTopEnumeration
      { HLIR.name = HLIR.MkAnnotation name generics,
        HLIR.constructors = constructors
      }
    )
  paths = do
    let newName = createName paths name
    pure
      [ HLIR.MkTopEnumeration
          { HLIR.name = HLIR.MkAnnotation newName generics,
            HLIR.constructors = Map.mapKeys (createName paths) constructors
          }
      ]
resolveModuleSingular (HLIR.MkTopPublic node) paths = do
  resolved <- resolveModuleSingular node paths
  pure (map HLIR.MkTopPublic resolved)
resolveModuleSingular (HLIR.MkTopExternalFunction {HLIR.name = (HLIR.MkAnnotation name generics), HLIR.parameters = params, HLIR.returnType = ret}) paths = do
  let newName = createName paths name

  let body =
        HLIR.MkExprVarCall
          name
          ( map
              ( \(HLIR.MkAnnotation paramName paramType) ->
                  HLIR.MkExprVariable (HLIR.MkAnnotation paramName (Just paramType)) []
              )
              params
          )

  pure
    [ HLIR.MkTopExternalFunction
        { HLIR.name = HLIR.MkAnnotation name generics,
          HLIR.parameters = params,
          HLIR.returnType = ret
        },
      HLIR.MkTopFunctionDeclaration
        { HLIR.name = HLIR.MkAnnotation newName generics,
          HLIR.parameters = params,
          HLIR.returnType = ret,
          HLIR.body = body
        }
    ]
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
