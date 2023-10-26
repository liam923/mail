{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Checked where

import Data.Map (Map)
import Data.String.Interpolate (i)

data Identifier = Identifier !String !Integer deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier name 0) = name
  show (Identifier name x) = [i|#{name}.#{x}|]

data UntypedExp
  = Let Identifier Exp Exp
  | VarRef Identifier
  | Eseq Exp Exp
  | Assign Identifier Exp
  | Ite Exp Exp Exp
  | FunCall Identifier [Exp]
  | StructMake Identifier [Exp]
  | StructDeref Exp Integer
  | UnionMake Identifier Integer Exp
  | Match Exp [MatchCase]
  | IntLiteral Integer
  | FloatLiteral Double
  | BoolLiteral Bool
  | UnitLiteral
  | UniOp UniOp Exp
  | BinOp BinOp Exp Exp
  deriving (Show)

data Exp = WithType {exp :: UntypedExp, type' :: TypeRef} deriving (Show)

data MatchCase = MatchCase {matchedConstructor :: Integer, caseBody :: Exp} deriving (Show)

data TypeRef
  = Int
  | Float
  | Bool
  | Unit
  | TypeRef Identifier
  deriving (Eq)

instance Show TypeRef where
  show Int = "Int"
  show Float = "Float"
  show Bool = "Bool"
  show Unit = "Unit"
  show (TypeRef t) = show t

data FunParam = FunParam {paramName :: Identifier, paramType :: TypeRef} deriving (Show)

data Fun = Fun [FunParam] TypeRef Exp deriving (Show)

data StructField = StructField {fieldName :: String, fieldType :: TypeRef} deriving (Show)

data UnionConstructor = UnionConstructor {constructorName :: String, constructorType :: TypeRef} deriving (Show)

data Type = Struct [StructField] | Union [UnionConstructor] deriving (Show)

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
