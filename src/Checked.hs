{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Checked where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.String.Interpolate (i)
import Data.Word (Word32)

data Identifier = Identifier {idName :: String, idCount :: Integer} deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier name 0) = name
  show (Identifier name x) = [i|#{name}.#{x}|]

data UntypedExp
  = Let Identifier Exp Exp
  | VarRef Identifier
  | Eseq Exp Exp
  | Assign Identifier Exp
  | Ite Exp Exp Exp
  | While Exp Exp
  | FunCall Identifier [Exp]
  | StructMake Identifier [Exp]
  | StructDeref Exp Word32
  | UnionMake Identifier Word32 Exp
  | Match Identifier Exp (NonEmpty MatchCase)
  | Alloc Exp
  | Dealloc Exp
  | SetPointer Exp Exp
  | GetPointer Exp
  | IntLiteral Integer
  | FloatLiteral Double
  | BoolLiteral Bool
  | UnitLiteral
  | UniOp UniOp Exp
  | BinOp BinOp Exp Exp
  deriving (Show)

data Exp = WithType {exp :: UntypedExp, type' :: TypeRef} deriving (Show)

data MatchCase = MatchCase {matchedConstructor :: Word32, matchedBinding :: Identifier, matchedType :: TypeRef, caseBody :: Exp} deriving (Show)

data TypeRef
  = Int
  | Float
  | Bool
  | Unit
  | TypePtr TypeRef
  | TypeRef Identifier
  deriving (Eq)

instance Show TypeRef where
  show Int = "Int"
  show Float = "Float"
  show Bool = "Bool"
  show Unit = "Unit"
  show (TypePtr t) = [i|(Ptr #{t})|]
  show (TypeRef t) = show t

data FunParam = FunParam {paramName :: Identifier, paramType :: TypeRef} deriving (Show)

data Fun = Fun [FunParam] TypeRef Exp deriving (Show)

data StructField = StructField {fieldName :: String, fieldType :: TypeRef} deriving (Show)

data UnionConstructor = UnionConstructor {constructorName :: String, constructorType :: TypeRef} deriving (Show)

data Type = Struct [StructField] | Union (NonEmpty UnionConstructor) deriving (Show)

data Program = Program {typeDefs :: Map Identifier Type, funDefs :: Map Identifier Fun, programBody :: Exp} deriving (Show)

data UniOp = NegateInt | NegateBool | NegateFloat deriving (Show)

data BinOp
  = IntPlus
  | IntMinus
  | IntMul
  | IntDiv
  | IntEQ
  | IntNE
  | IntLT
  | IntLE
  | IntGT
  | IntGE
  | FloatPlus
  | FloatMinus
  | FloatMul
  | FloatDiv
  | FloatEQ
  | FloatNE
  | FloatLT
  | FloatLE
  | FloatGT
  | FloatGE
  | And
  | Or
  deriving (Show)
