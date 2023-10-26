{-# LANGUAGE QuasiQuotes #-}

module Compiler (compile, printLLVM, writeLLVM) where

import Data.String.Interpolate (i)
import qualified Data.Text.Lazy.IO as T
import qualified LLVM.AST
import LLVM.Pretty (ppllvm)
import Paths_mail (getDataFileName)
import System.Process (callProcess)

printLLVM :: LLVM.AST.Module -> IO ()
printLLVM modl = T.putStr $ ppllvm modl

writeLLVM :: FilePath -> LLVM.AST.Module -> IO ()
writeLLVM path modl = T.writeFile path $ ppllvm modl

compile :: String -> LLVM.AST.Module -> IO FilePath
compile name modl = do
  let llvmPath = [i|#{name}.ll|]
  writeLLVM llvmPath modl
  runtimePath <- getDataFileName "runtime.c"
  let outputPath = name
  callProcess
    "clang"
    ["-Wno-override-module", "-lm", llvmPath, runtimePath, "-o", outputPath]
  pure outputPath
