module Main where

import Checker
import Compiler
import Generator
import Options.Applicative
import Parser
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.FilePath (dropExtension, takeFileName)
import System.Process (callProcess)

data CommandArgs = CommandArgs {inputFile :: (Maybe String), outputDir :: (Maybe String), llvmOnly :: Bool, shouldRun :: Bool}

argParser :: Parser CommandArgs
argParser =
  CommandArgs
    <$> optional
      ( argument
          str
          ( metavar "input-file"
              <> help "The Mail program to compile"
          )
      )
    <*> optional
      ( strOption
          ( long "outdir"
              <> short 'o'
              <> help "The directory to write output files to"
          )
      )
    <*> switch
      ( long "llvm-only"
          <> short 'l'
          <> help "Include this flag to only produce llvm and not compile a binary"
      )
    <*> switch
      ( long "run"
          <> short 'r'
          <> help "Include this flag to run the program after compiling"
      )

run :: CommandArgs -> IO ()
run args = do
  (programName, programString) <- case inputFile args of
    Just filename -> readFile filename >>= \c -> pure (dropExtension . takeFileName $ filename, c)
    Nothing -> getContents >>= \c -> pure ("stdin", c)

  program <- case parseProgram programName programString >>= check of
    Right program -> pure program
    Left errorMessage -> ioError $ userError errorMessage

  outDir <- maybe getCurrentDirectory pure (outputDir args)

  (_, binPath) <- compile programName outDir (llvmOnly args) $ gen programName program
  absBinPath <- makeAbsolute binPath

  (if shouldRun args then callProcess absBinPath [] else pure ())

main :: IO ()
main = do
  run =<< execParser opts
  where
    opts =
      info
        (argParser <**> helper)
        ( fullDesc
            <> progDesc "Compile a Mail program"
            <> header "mailc - a compiler for Mail"
        )
