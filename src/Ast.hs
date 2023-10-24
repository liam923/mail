module Ast where

data Exp
  = Let String Exp Exp
  | VarRef String
  | Eseq Exp Exp
  | Assign String Exp
  | Ite Exp Exp Exp
  | Call String [Exp]
  | StructDeref Exp String
  | Match Exp [MatchCase]
  | IntLiteral Integer
  | FloatLiteral Double
  | BoolLiteral Bool
  | UniOp UniOp Exp
  | BinOp BinOp Exp Exp
  deriving (Show)

data MatchCase = MatchCase {matchedConstructor :: String, matchedBinding :: String, caseBody :: Exp} deriving (Show)

data TypeRef = Int | Float | Bool | Unit | TypeRef String deriving (Eq, Show)

data FunParam = FunParam {paramName :: String, paramType :: TypeRef} deriving (Show)

data StructField = StructField {fieldName :: String, fieldType :: TypeRef} deriving (Show)

data UnionConstructor = UnionConstructor {constructorName :: String, constructorType :: TypeRef} deriving (Show)

data Type = Struct [StructField] | Union [UnionConstructor] deriving (Show)

data Definition
  = FunDef String [FunParam] TypeRef Exp
  | StructDef String [StructField]
  | UnionDef String [UnionConstructor]
  deriving (Show)

data Program = Program [Definition] Exp deriving (Show)

data UniOp = NegateInt | NegateBool deriving (Show)

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
