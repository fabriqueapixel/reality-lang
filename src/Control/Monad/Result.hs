{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Result where

import Control.Color
import Control.Monad.Except
import Data.Maybe qualified as Mb
import Data.Text qualified as Text
import Error.Diagnose qualified as D
import Error.Diagnose.Compat.Megaparsec qualified as D
import GHC.IO qualified as IO
import GHC.Show qualified as Show
import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Frontend.Typechecker.Monad qualified as TC
import Language.Reality.Syntax.HLIR qualified as HLIR
import System.Directory (doesFileExist)
import System.FilePath (normalise)
import Text.Megaparsec hiding (parseError)

instance (D.HasHints Void String) where
    hints _ = mempty

fromEither :: a -> Either b a -> a
fromEither _ (Right a) = a
fromEither a _ = a

handle :: (MonadIO m) => Either Error b -> (b -> m c) -> m c
handle (Right a) f = f a
handle (Left (err, pos@(p1, _))) _ = liftIO $ do
    case err of
        ParseError e -> parseError e (P.sourceName p1) Nothing
        CyclicModuleDependency path stack ->
            printErrorFromString
                Nothing
                ( "Cyclic module dependency detected with " <> show (normalise path)
                , Nothing
                , pos
                )
                stackMsg
          where
            stackMsg = "Import stack:\n - " <> intercalate "\n - " (map normalise stack)
        ModuleNotFound path ->
            printErrorFromString
                Nothing
                ( "Module " <> show (normalise path) <> " not found"
                , Just "check for typo issue with the module name"
                , pos
                )
                "Resolution"
        VariableNotFound name ->
            printErrorFromString
                Nothing
                ( "Variable " <> show name <> " not found"
                , Just "check for typo issue with the variable"
                , pos
                )
                "Resolution"
        CompilerError msg ->
            printErrorFromString
                Nothing
                ( "Reality INTERNAL ERROR: " <> show msg
                , Just "report the issue to Reality developers"
                , pos
                )
                "Resolution"
        UnificationFail got expected ->
            printErrorFromString
                Nothing
                ( "Expected "
                    <> toString (toText expected)
                    <> ", but got "
                    <> toString (toText got)
                , Just "ensure the types are compatible, or check for missing type annotations"
                , pos
                )
                ("Expected type " <> toString (toText expected))
        InvalidArgumentQuantity n k ->
            printErrorFromString
                Nothing
                ( "Invalid number of arguments, expected " <> show n <> ", received " <> show k
                , Nothing
                , pos
                )
                "Resolution"
        EnvironmentVariableNotFound name ->
            printErrorFromString
                Nothing
                ( "Environment variable " <> show name <> " not found"
                , Just "check for typo issue with the variable name"
                , pos
                )
                "Resolution"
        FieldNotFound field ->
            printErrorFromString
                Nothing
                ( "Field " <> show field <> " not found in structure"
                , Nothing
                , pos
                )
                "Resolution"
        UnboundTypeVariable name ->
            printErrorFromString
                Nothing
                ( "Unbound type variable " <> show name
                , Nothing
                , pos
                )
                "Resolution"
        StructureNotFound ty ->
            printErrorFromString
                Nothing
                ( "Structure " <> show (toText ty) <> " not found"
                , Nothing
                , pos
                )
                "Resolution"
        CyclicTypeVariable name ty ->
            printErrorFromString
                Nothing
                ( "Cyclic type variable " <> show name <> " in type " <> show (toText ty)
                , Nothing
                , pos
                )
                "Resolution"
        ExpectedFunction ty ->
            printErrorFromString
                Nothing
                ( "Expected a function type, but got " <> show (toText ty)
                , Nothing
                , pos
                )
                "Resolution"
        InvalidHeader ->
            printErrorFromString
                Nothing
                ( "Invalid header type"
                , Just "ensure the type is a valid structure type"
                , pos
                )
                "Resolution"
        PropertyNotFound name ->
            printErrorFromString
                Nothing
                ( "Property " <> show name <> " not found"
                , Nothing
                , pos
                )
                "Resolution"
        UnsolvedConstraints cs ->
            printErrorFromString
                Nothing
                ( "Unsolved constraints:\n"
                    <> Text.unpack
                        ( Text.unlines
                            ( map
                                ( \case
                                    TC.MkFieldConstraint ty field fieldTy _ ->
                                        " - field "
                                            <> field
                                            <> " of type "
                                            <> toText fieldTy
                                            <> " in type "
                                            <> toText ty
                                    TC.MkImplConstraint name ty _ _ -> " - implementation of property " <> name <> " for type " <> toText ty
                                )
                                cs
                            )
                        )
                , Nothing
                , pos
                )
                "Resolution"
        ImplementationNotFound name ty ->
            printErrorFromString
                Nothing
                ( "Implementation for property "
                    <> show name
                    <> " and type "
                    <> show (toText ty)
                    <> " not found"
                , Nothing
                , pos
                )
                "Resolution"
        NoFunctionInStructure name ty ->
            printErrorFromString
                Nothing
                ( "Functions are not permitted in structures (got field "
                    <> show name
                    <> ": "
                    <> show (toText ty)
                    <> ")"
                , Just "use instead properties or functions outside structures"
                , pos
                )
                "Resolution"
        ReturnOutsideFunction ->
            printErrorFromString
                Nothing
                ( "Return statement outside of a function"
                , Just "ensure the return statement is inside a function body"
                , pos
                )
                "Resolution"
        BreakOutsideLoop ->
            printErrorFromString
                Nothing
                ( "Break or continue statement outside of a loop"
                , Just "ensure the break statement is inside a loop body"
                , pos
                )
                "Resolution"
        RecursionLimitExceeded _ ->
            printErrorFromString
                Nothing
                ( "Infinite recursion detected"
                , Just "you may have written some infinitely recursive data-type"
                , pos
                )
                "Resolution"
        RewriteRowError ty label' ->
            printErrorFromString
                Nothing
                ( "Cannot rewrite row type " <> show (toText ty) <> " with label " <> show label'
                , Nothing
                , pos
                )
                "Resolution"
        ExpectedRecord ty ->
            printErrorFromString
                Nothing
                ( "Expected a record type, but got " <> show (toText ty)
                , Nothing
                , pos
                )
                "Resolution"

type ImportStack = [FilePath]

type Error = (RealityError, HLIR.Position)

annotateErrorBundle :: ParseErrorBundle Text Void -> NonEmpty (SourcePos, Text)
annotateErrorBundle bundle =
    fmap (\(err, pos) -> (pos, Text.pack . parseErrorTextPretty $ err)) . fst
        $ attachSourcePos
            errorOffset
            (bundleErrors bundle)
            (bundlePosState bundle)

data RealityError
    = ParseError P.ParseError
    | CyclicModuleDependency FilePath ImportStack
    | ModuleNotFound FilePath
    | VariableNotFound Text
    | CompilerError Text
    | UnificationFail HLIR.Type HLIR.Type
    | CyclicTypeVariable Text HLIR.Type
    | InvalidArgumentQuantity Int Int
    | FieldNotFound Text
    | UnboundTypeVariable Text
    | StructureNotFound HLIR.Type
    | EnvironmentVariableNotFound Text
    | ExpectedFunction HLIR.Type
    | InvalidHeader
    | PropertyNotFound Text
    | UnsolvedConstraints TC.Constraints
    | ImplementationNotFound Text HLIR.Type
    | NoFunctionInStructure Text HLIR.Type
    | ReturnOutsideFunction
    | BreakOutsideLoop
    | RecursionLimitExceeded Integer
    | RewriteRowError HLIR.Type Text
    | ExpectedRecord HLIR.Type
    deriving (Eq, Generic)

instance Show RealityError where
    show (ParseError e) = showError e
    show (CyclicModuleDependency path []) = "Cyclic module dependency detected with " <> show (normalise path)
    show (CyclicModuleDependency path stack) =
        "Cyclic module dependency detected with "
            <> show (normalise path)
            <> "\nImport stack:\n - "
            <> intercalate "\n - " (map normalise stack)
    show (ModuleNotFound path) =
        "Module "
            <> show (normalise path)
            <> " not found"
    show (VariableNotFound name) = "Variable " <> show name <> " not found"
    show (CompilerError msg) = "Reality INTERNAL ERROR: " <> show msg
    show (UnificationFail t1 t2) = "Expected " <> show (toText t1) <> ", but got " <> show (toText t2)
    show (InvalidArgumentQuantity n k) = "Invalid number of arguments, expected " <> show n <> ", received " <> show k
    show (EnvironmentVariableNotFound name) = "Environment variable " <> show name <> " not found"
    show (FieldNotFound field) = "Field " <> show field <> " not found in structure"
    show (UnboundTypeVariable name) = "Unbound type variable " <> show name
    show (StructureNotFound ty) = "Structure " <> show (toText ty)
    show (CyclicTypeVariable name ty) = "Cyclic type variable " <> show name <> " in type " <> show (toText ty)
    show (ExpectedFunction ty) =
        "Expected a function type, but got "
            <> show
                (toText ty)
    show InvalidHeader = "Invalid header type"
    show (PropertyNotFound name) = "Property " <> show name <> " not found"
    show (UnsolvedConstraints cs) =
        "Unsolved constraints:\n"
            <> Text.unpack
                ( Text.unlines
                    ( map
                        ( \case
                            TC.MkFieldConstraint ty field fieldTy _ ->
                                " - field "
                                    <> field
                                    <> " of type "
                                    <> toText fieldTy
                                    <> " in type "
                                    <> toText ty
                            TC.MkImplConstraint name ty _ _ -> " - implementation of property " <> name <> " for type " <> toText ty
                        )
                        cs
                    )
                )
    show (ImplementationNotFound name ty) =
        "Implementation for property "
            <> show name
            <> " and type "
            <> show (toText ty)
            <> " not found"
    show (NoFunctionInStructure name ty) =
        "Functions are not permitted in structures (got field "
            <> show name
            <> ": "
            <> show (toText ty)
            <> ")"
    show ReturnOutsideFunction = "Return statement outside of a function"
    show BreakOutsideLoop = "Break or continue statement outside of a loop"
    show (RecursionLimitExceeded _) =
        "Recursion limit exceeded"
    show (RewriteRowError ty label') =
        "Cannot rewrite row type " <> show (toText ty) <> " with label " <> show label'
    show (ExpectedRecord ty) =
        "Expected a record type, but got " <> show (toText ty)
    
showError :: P.ParseError -> String
showError = P.errorBundlePretty

compilerError :: (HasCallStack) => Text -> a
compilerError msg = do
    let err = "Reality INTERNAL ERROR: " <> msg

    let cs = getCallStack callStack
        callstack = Text.unlines $ map (("    - " <>) . fromString . prettySrcLoc . snd) cs
        pCallstack =
            if null cs
                then ""
                else "\n  A bug occured in Reality compiler.\n  CallStack:\n" <> callstack

    IO.unsafePerformIO $ do
        putStrLn . toString $ err <> pCallstack
        exitFailure

throw :: (MonadError Error m, MonadIO m) => RealityError -> m a
throw e = do
    pos <- HLIR.peekPosition'
    throwError (e, pos)

parseError :: P.ParsingError -> FilePath -> Maybe P.FileContent -> IO a
parseError err' _ fc = do
    let diag :: D.Diagnostic String = D.errorDiagnosticFromBundle Nothing "Parse error on input" Nothing err'

    let fp' = err'.bundlePosState.pstateSourcePos.sourceName

    b <- doesFileExist fp'

    content' <- readFileBS fp'
    let contentAsText = decodeUtf8 content'

    let x' = toString $ if b then contentAsText else Mb.fromJust fc
        diag' = D.addFile diag fp' x'
     in do
            D.printDiagnostic stdout True True 4 D.defaultStyle diag'
            exitFailure

printErrorFromString ::
    Maybe Text -> (String, Maybe String, HLIR.Position) -> String -> IO a
printErrorFromString content (error', msg, (p1, p2)) step = do
    let p1' = (P.unPos p1.sourceLine, P.unPos p1.sourceColumn)
    let p2' = (P.unPos p2.sourceLine, P.unPos p2.sourceColumn)
    let file' = p1.sourceName
    b <- doesFileExist file'

    content' <- if b then readFileBS file' else pure ""
    let contentAsText = decodeUtf8 content'

    let x' = toString $ if b then contentAsText else Mb.fromJust content
    let pos' = D.Position p1' p2' p1.sourceName
    let beautifulExample =
            D.err
                Nothing
                error'
                [(pos', D.This step)]
                (maybeToList msg)

    -- Create the diagnostic
    let diagnostic = D.addFile D.def file' x'
    let diagnostic' = D.addReport diagnostic beautifulExample

    -- Print with unicode characters, colors and the default style
    D.printDiagnostic stdout True True 4 D.defaultStyle diagnostic'
    exitFailure

ppError :: (ToString a) => a -> IO b
ppError t = IO.unsafePerformIO $ do
    putStrLn $ colorBold Red "[error]: " <> toString t

    exitFailure

ppSuccess :: (ToString a) => a -> IO ()
ppSuccess t = putStrLn $ colorBold Green "[success]: " <> toString t

ppWarning :: (ToString a) => a -> IO ()
ppWarning t = putStrLn $ colorBold Yellow "[warning]: " <> toString t

ppBuild :: (ToString a) => a -> IO ()
ppBuild t = putStrLn $ colorBold Cyan "[build]: " <> toString t

(|>) :: (MonadError err m, MonadIO m) => (a -> m b) -> (b -> m c) -> a -> m c
(|>) f g x = f x >>= g

infixl 1 |>
