module Main where

import Checker
import Compiler
import Generator
import Parser
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeFileName)

main :: IO ()
main = do
  args <- getArgs

  (filename, programString) <- case args of
    [filename] -> readFile filename >>= \c -> pure (dropExtension . takeFileName $ filename, c)
    [] -> getContents >>= \c -> pure ("stdin", c)
    _ -> ioError $ userError "Expected 0 or 1 arguments"

  program <- case parseProgram filename programString >>= check of
    Right program -> pure program
    Left errorMessage -> ioError $ userError errorMessage

  _ <- compile filename $ gen filename program
  pure ()
