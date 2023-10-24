module Main where

import Checker
import Parser (parseProgram)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  (filename, programString) <- case args of
    [filename] -> readFile filename >>= \c -> return (filename, c)
    [] -> getContents >>= \c -> return ("stdin", c)
    _ -> ioError $ userError "Expected 0 or 1 arguments"

  () <- case parseProgram filename programString >>= check of
    Right _ -> return ()
    Left errorMessage -> ioError $ userError errorMessage
  print "Success!"
