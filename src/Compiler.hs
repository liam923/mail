{-# LANGUAGE QuasiQuotes #-}

module Compiler (compile, printLLVM, writeLLVM) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.String.Interpolate (i)
import LLVM (withModuleFromAST)
import LLVM.AST (Module)
import LLVM.Context (withContext)
import LLVM.Internal.Module (moduleLLVMAssembly)
import Paths_mail (getDataFileName)
import System.Process (callProcess)

llvmToBS :: Module -> IO ByteString
llvmToBS modl = withContext $ \ctx ->
  withModuleFromAST ctx modl moduleLLVMAssembly

printLLVM :: Module -> IO ()
printLLVM modl = llvmToBS modl >>= BS.putStr

writeLLVM :: FilePath -> LLVM.AST.Module -> IO ()
writeLLVM path modl = llvmToBS modl >>= BS.writeFile path

compile :: String -> Module -> IO FilePath
compile name modl = do
  let llvmPath = [i|#{name}.ll|]
  writeLLVM llvmPath modl
  runtimePath <- getDataFileName "runtime.c"
  let outputPath = name
  callProcess
    "clang"
    ["-Wno-override-module", "-lm", llvmPath, runtimePath, "-o", outputPath]
  pure outputPath
