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
  | IntLiteral Int
  | FloatLiteral Float
  | BoolLiteral Bool
  | UniOp UniOp Exp
  | BinOp BinOp Exp Exp

data MatchCase = MatchCase {matchedConstructor :: String, matchedBinding :: String, caseBody :: Exp}

data TypeRef = Int | Float | Bool | Unit | TypeRef String deriving (Eq, Show)

data FunParam = FunParam {paramName :: String, paramType :: TypeRef}

data StructField = StructField {fieldName :: String, fieldType :: TypeRef}

data UnionConstructor = UnionConstructor {constructorName :: String, constructorType :: TypeRef}

data Type = Struct [StructField] | Union [UnionConstructor]

data Definition
  = FunDef String [FunParam] TypeRef Exp
  | StructDef String [StructField]
  | UnionDef String [UnionConstructor]

type Program = [Definition]

data UniOp = NegateInt | NegateBool

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
