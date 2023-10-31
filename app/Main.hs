module Main where

import Checker
import Compiler
import Generator
import Options.Applicative
import Parser
import System.Directory (makeAbsolute)
import System.FilePath (dropExtension, takeFileName)
import System.Process (callProcess)

data CommandArgs = CommandArgs (Maybe String) Bool

argParser :: Parser CommandArgs
argParser =
  CommandArgs
    <$> optional (argument str (metavar "input-file" <> help "The Mail program to compile"))
    <*> switch
      ( long "run"
          <> short 'r'
          <> help "Whether to run the program after compiling"
      )

run :: CommandArgs -> IO ()
run (CommandArgs filenameMaybe shouldRun) = do
  (programName, programString) <- case filenameMaybe of
    Just filename -> readFile filename >>= \c -> pure (dropExtension . takeFileName $ filename, c)
    Nothing -> getContents >>= \c -> pure ("stdin", c)

  program <- case parseProgram programName programString >>= check of
    Right program -> pure program
    Left errorMessage -> ioError $ userError errorMessage

  binPath <- compile programName $ gen programName program
  absBinPath <- makeAbsolute binPath

  (if shouldRun then callProcess absBinPath [] else pure ())

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
