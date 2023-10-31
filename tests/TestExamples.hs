{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Checker
import qualified Compiler
import Data.String.Interpolate (i)
import qualified Data.Text as T
import GHC.IO.Exception (ExitCode (ExitSuccess))
import qualified Generator
import qualified Parser
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process (readProcessWithExitCode)

data Example = Example String Integer

examples :: [Example]
examples =
  [ Example "simple" 10,
    Example "fibonacci-rec" 13,
    Example "fibonacci-iter" 13,
    Example "fibonacci-dp" 13,
    Example "project-euler1" 233168,
    Example "project-euler2" 4613732,
    Example "project-euler3" 6857,
    Example "project-euler5" 232792560,
    Example "project-euler6" 25164150
  ]

getExamplesDir :: IO FilePath
getExamplesDir = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "examples"

withTempCurrentDirectory :: String -> IO a -> IO a
withTempCurrentDirectory name body = do
  withSystemTempDirectory name $ \temp -> withCurrentDirectory temp body

verifyExample :: FilePath -> Example -> IO ()
verifyExample examplesDir (Example exampleName expected) = do
  let programPath = examplesDir </> addExtension exampleName "mail"
  let expectedLlvmPath = examplesDir </> addExtension exampleName "ll"
  withSystemTempDirectory exampleName $ \tempDir -> do
    programString <- readFile programPath
    program <- case Parser.parseProgram exampleName programString >>= Checker.check of
      Right program -> pure program
      Left errorMessage -> ioError $ userError errorMessage

    (actualLlvmPath, binPath) <- Compiler.compile exampleName tempDir False $ Generator.gen exampleName program
    absBinPath <- makeAbsolute binPath

    expectedLlvm <- readFile expectedLlvmPath
    actualLlvm <- readFile actualLlvmPath
    (if expectedLlvm == actualLlvm then pure () else ioError $ userError "llvm does not match")

    (errCode, stdout', stderr') <- readProcessWithExitCode absBinPath [] ""
    actualOutput <-
      ( if errCode == ExitSuccess
          then pure $ T.unpack . T.strip . T.pack $ stdout'
          else ioError . userError $ [i|Error running test #{exampleName} - Program failed to run with output:\n#{stderr'}|]
        )
    let expectedOutput = show expected
    ( if expectedOutput == actualOutput
        then pure ()
        else ioError $ userError [i|Error running test #{exampleName} - Expected #{expectedOutput}, got #{actualOutput}|]
      )

    pure ()

main :: IO ()
main = do
  examplesDir <- getExamplesDir
  mapM_ (verifyExample examplesDir) examples
