module Main where

import Checker
import Generator
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  (filename, programString) <- case args of
    [filename] -> readFile filename >>= \c -> pure (filename, c)
    [] -> getContents >>= \c -> pure ("stdin", c)
    _ -> ioError $ userError "Expected 0 or 1 arguments"

  program <- case parseProgram filename programString >>= check of
    Right program -> pure program
    Left errorMessage -> ioError $ userError errorMessage

  printLLVM $ gen program

  print "Success!"
