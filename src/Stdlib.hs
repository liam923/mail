module Stdlib where

import Checked
import Data.Map (Map)
import qualified Data.Map as Map

data EnvEntry = UniOpEntry UniOp TypeRef TypeRef | BinOpEntry BinOp TypeRef TypeRef TypeRef

stdlib :: Map String EnvEntry
stdlib =
  let entries =
        map (\(k, v, arg1, arg2, ret) -> (k, BinOpEntry v arg1 arg2 ret)) binOps
          ++ map (\(k, v, arg, ret) -> (k, UniOpEntry v arg ret)) uniOps
   in Map.fromList entries
  where
    binOps =
      [ ("+", IntPlus, Int, Int, Int),
        ("-", IntMinus, Int, Int, Int),
        ("*", IntMul, Int, Int, Int),
        ("/", IntDiv, Int, Int, Int),
        ("%", IntMod, Int, Int, Int),
        ("=", IntEQ, Int, Int, Bool),
        ("!=", IntNE, Int, Int, Bool),
        ("<", IntLT, Int, Int, Bool),
        ("<=", IntLE, Int, Int, Bool),
        (">", IntGT, Int, Int, Bool),
        (">=", IntGE, Int, Int, Bool),
        ("+.", FloatPlus, Float, Float, Float),
        ("-.", FloatMinus, Float, Float, Float),
        ("*.", FloatMul, Float, Float, Float),
        ("/.", FloatDiv, Float, Float, Float),
        ("=.", FloatEQ, Float, Float, Bool),
        ("!=.", FloatNE, Float, Float, Bool),
        ("<.", FloatLT, Float, Float, Bool),
        ("<=.", FloatLE, Float, Float, Bool),
        (">.", FloatGT, Float, Float, Bool),
        (">=.", FloatGE, Float, Float, Bool),
        ("and", And, Bool, Bool, Bool),
        ("or", Or, Bool, Bool, Bool)
      ]
    uniOps =
      [ ("not", NegateBool, Bool, Bool)
      ]
