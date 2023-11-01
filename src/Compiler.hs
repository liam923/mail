module Compiler (compile, printLLVM, writeLLVM) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import LLVM (withModuleFromAST)
import LLVM.AST (Module)
import LLVM.Context (withContext)
import LLVM.Internal.Module (moduleLLVMAssembly)
import Paths_mail (getDataFileName)
import System.FilePath (addExtension, (</>))
import System.Process (callProcess)

llvmToBS :: Module -> IO ByteString
llvmToBS modl = withContext $ \ctx ->
  withModuleFromAST ctx modl moduleLLVMAssembly

printLLVM :: Module -> IO ()
printLLVM modl = llvmToBS modl >>= BS.putStr

writeLLVM :: FilePath -> LLVM.AST.Module -> IO ()
writeLLVM path modl = llvmToBS modl >>= BS.writeFile path

compile :: String -> FilePath -> Bool -> Module -> IO (FilePath, FilePath)
compile name outDir llvmOnly modl = do
  let llvmPath = outDir </> addExtension name "ll"
  writeLLVM llvmPath modl
  runtimePath <- getDataFileName "runtime.c"
  let outputPath = outDir </> name
  ( if llvmOnly
      then pure ()
      else
        callProcess
          "clang"
          ["-Wno-override-module", "-lm", llvmPath, runtimePath, "-o", outputPath]
    )
  pure (llvmPath, outputPath)
