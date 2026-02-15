module Main where

import Control.Monad.Result
import Language.Reality.Backend.ANF.Converter qualified as ANF
import Language.Reality.Backend.Closure.Converter qualified as CC
import Language.Reality.Backend.Codegen qualified as CG
import Language.Reality.Backend.Specialization.Resolver qualified as SR
import Language.Reality.Frontend.Import.Resolver qualified as IR
import Language.Reality.Frontend.Module.Resolver qualified as MR
import Language.Reality.Frontend.Parser hiding (parseError, Parser)
import Language.Reality.Frontend.Parser.Toplevel qualified as T
import Language.Reality.Frontend.Typechecker.Checker qualified as TC
import System.Directory
import System.FilePath
import System.Process

import Options.Applicative qualified as App
import qualified Data.Text as Text
import System.Environment (getEnv)

data BuildOptions
    = MkBuildRealityOptions
        { optInputFile :: FilePath
        , optPathAliases :: [(Text, FilePath)]
        , optHeaders :: [FilePath]
        }
    | MkBuildClangOptions
        { clangInputFile :: FilePath
        , clangOutputFile :: FilePath
        , clangLibraries :: [FilePath]
        , clangFlags :: [String]
        }

options :: App.Parser BuildOptions
options = MkBuildRealityOptions
    <$> App.strArgument
        ( App.metavar "FILE"
       <> App.help "Reality source file to compile" )
    <*> App.many (App.option (App.eitherReader parseAlias)
        ( App.long "path-alias"
       <> App.short 'p'
       <> App.metavar "ALIAS=PATH"
       <> App.help "Path alias for import resolution" ))
    <*> App.many (App.strOption
        ( App.long "header"
       <> App.short 'I'
       <> App.metavar "HEADER"
       <> App.help "Additional header file to include in the generated C code" ))
  where
    parseAlias :: String -> Either String (Text, FilePath)
    parseAlias s =
        case break (== '=') s of
            (alias, '=':path) -> Right (Text.pack alias, path)
            _ -> Left "Alias must be in the format ALIAS=PATH"

clangOptions :: App.Parser BuildOptions
clangOptions = MkBuildClangOptions
    <$> App.strArgument
        ( App.metavar "INPUT"
       <> App.help "Input C file to compile" )
    <*> App.strOption
        ( App.long "output"
       <> App.short 'o'
       <> App.metavar "OUTPUT"
       <> App.help "Output executable file name" 
       <> App.value "output/program")
    <*> App.many (App.strOption
        ( App.long "library"
       <> App.short 'l'
       <> App.metavar "LIBRARY"
       <> App.help "Additional library to link against" ))
    <*> App.many (App.strOption
        ( App.long "flag"
       <> App.short 'f'
       <> App.metavar "FLAG"
       <> App.help "Additional flag to pass to the C compiler" ))
main :: IO ()
main = do
    buildOpts <- App.execParser (App.info p App.idm)
    buildOutput buildOpts
  where
    p = App.hsubparser
        ( App.command "c" cOpts )
        <|> options 

    cOpts = App.info (App.helper <*> clangOptions)
      ( App.fullDesc
     <> App.progDesc "Compile a Reality output C file to an executable using C compiler"
     <> App.header "hello - a test for optparse-applicative" )


buildOutput :: BuildOptions -> IO ()
buildOutput (MkBuildRealityOptions inputFile pathAliases headers) = do
    let file = inputFile
        cwd  = takeDirectory file

    fileContent :: Text <- decodeUtf8 <$> readFileBS file

    result <- parseRealityFile file fileContent T.parseProgram

    let pathAliases' = fromList pathAliases
    pathAliases'' <- mapM makeAbsolute pathAliases'

    case result of
        Right ast -> do
            let pipeline =
                    IR.runImportResolver cwd pathAliases''
                        |> MR.runModuleResolver
                        |> TC.runTypechecker
                        |> SR.runSpecializationResolver
                        |> CC.convertProgram
                        |> ANF.convertToANF
                        |> CG.codegenProgram

            pipelineResult <- runExceptT $ pipeline ast

            let includes = ["<stdint.h>", "<stdbool.h>", "<pthread.h>", "<gc.h>"]
            let headers' = map (\h -> do
                        let path = toText h
                        if Text.isPrefixOf "<" path && Text.isSuffixOf ">" path
                            then "#include " <> path
                            else "#include \"" <> path <> "\""
                    ) headers
            let defines = ["#define GC_THREADS"]

            handle pipelineResult $ \cstr -> do
                let finalCstr =
                        unlines defines
                        <> unlines (map ("#include " <>) includes)
                        <> unlines headers'
                        <> "\n\n"
                        <> cstr

                writeFileText (file -<.> "c") finalCstr
        Left err -> do
            parseError err file (Just fileContent)
buildOutput (MkBuildClangOptions inputFile outputFile libraries flags) = do
    realityDir <- getEnv "REALITY_DIR"
    libgcDir <- getEnv "LIBGC"

    let clangCmd = "clang"
        args = 
            [inputFile, "-o", outputFile] 
            ++ libraries 
            ++ [
                realityDir <> "/libc/String.c", 
                realityDir <> "/libc/Actor.c", 
                libgcDir <> "/lib/libgc.a", 
                "-I" <> libgcDir <> "/include/", 
                "-w"
            ]
            ++ map ('-':) flags

    ppBuild $ "Running clang with arguments: " <> unwords (map fromString args)
    callProcess clangCmd args
