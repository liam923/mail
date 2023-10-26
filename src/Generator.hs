{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Generator where

import Checked
import Data.ByteString.Char8 as BS
import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import LLVM.AST.Type as AST
import LLVM.Context
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad (block, named)
import qualified LLVM.Module as LLVM

simple :: LLVM.AST.Module
simple = buildModule "exampleModule" $ mdo
  _ <- function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
    _entry <- block `named` "entry"
    cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
    condBr cond ifThen ifElse
    ifThen <- block
    trVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifElse <- block `named` "if.else"
    flVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifExit <- block `named` "if.exit"
    r <- phi [(trVal, ifThen), (flVal, ifElse)]
    ret r

  function "plus" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    _entry <- block `named` "entry2"
    r <- add x y
    ret r

printLLVM :: Module -> IO ()
printLLVM modl = withContext $ \ctx -> do
  llvm <- LLVM.withModuleFromAST ctx modl LLVM.moduleLLVMAssembly
  BS.putStrLn llvm

gen :: Program -> LLVM.AST.Module
gen _ =
  simple
