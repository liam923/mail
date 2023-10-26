{-# LANGUAGE QuasiQuotes #-}

module Compiler where

import qualified Data.ByteString.Char8 as BS
import Data.String.Interpolate (i)
import qualified LLVM
import qualified LLVM.AST
import qualified LLVM.Context
import Paths_mail (getDataFileName)
import System.Process (callProcess)

bsLLVM :: LLVM.AST.Module -> IO BS.ByteString
bsLLVM modl = LLVM.Context.withContext $ \ctx ->
  LLVM.withModuleFromAST ctx modl LLVM.moduleLLVMAssembly

printLLVM :: LLVM.AST.Module -> IO ()
printLLVM modl = bsLLVM modl >>= BS.putStrLn

writeLLVM :: FilePath -> LLVM.AST.Module -> IO ()
writeLLVM path modl = bsLLVM modl >>= BS.writeFile path

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
