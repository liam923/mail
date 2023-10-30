module Main where

import Checker
import Compiler
import Generator
import Parser
import System.Directory (makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeFileName)
import System.Process (callProcess)

main :: IO ()
main = do
  args <- getArgs

  (programName, programString) <- case args of
    [filename] -> readFile filename >>= \c -> pure (dropExtension . takeFileName $ filename, c)
    [] -> getContents >>= \c -> pure ("stdin", c)
    _ -> ioError $ userError "Expected 0 or 1 arguments"

  program <- case parseProgram programName programString >>= check of
    Right program -> pure program
    Left errorMessage -> ioError $ userError errorMessage

  print program

  binPath <- compile programName $ gen programName program
  absBinPath <- makeAbsolute binPath

  callProcess absBinPath []
