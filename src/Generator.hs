{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Generator where

import Checked
import Data.ByteString.Short as BLU
import Data.String.Interpolate (i)
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad (block, named)

-- import qualified LLVM.Module as LLVM

-- simple :: LLVM.AST.Module
-- simple = buildModule "exampleModule" $ mdo
--   _ <- function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
--     _entry <- block `named` "entry"
--     cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
--     condBr cond ifThen ifElse
--     ifThen <- block
--     trVal <- add a (ConstantOperand (C.Int 32 0))
--     br ifExit
--     ifElse <- block `named` "if.else"
--     flVal <- add a (ConstantOperand (C.Int 32 0))
--     br ifExit
--     ifExit <- block `named` "if.exit"
--     r <- phi [(trVal, ifThen), (flVal, ifElse)]
--     ret r

--   function "plus" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
--     _entry <- block `named` "entry2"
--     r <- add x y
--     ret r

-- declare void @printNum(i64 %n)

-- define i32 @main() {
--   call void (i64) @printNum(i64 69420)
--   ret i32 0
-- }

gen :: String -> Program -> LLVM.AST.Module
gen progName _ =
  buildModule [i|#{progName}|] $ mdo
    printNum <- extern "printNum" [AST.i64] AST.VoidType

    function "main" [(AST.i32, "a"), (AST.i32, "a")] AST.i32 $ \_ -> do
      -- _ <- add (C.int32 10) (LocalReference AST.i32 "hi")
      _ <- call printNum [(C.int64 69420, [])]
      ret (C.int32 0)
