{-# LANGUAGE StrictData #-}

module Ast where

data Exp
  = Let String Exp Exp
  | VarRef String
  | Eseq Exp Exp
  | Assign String Exp
  | Ite Exp Exp Exp
  | While Exp Exp
  | Call String [Exp]
  | Match Exp [MatchCase]
  | Alloc TypeRef Exp
  | Dealloc Exp
  | SetPointer Exp Exp
  | IntLiteral Integer
  | FloatLiteral Double
  | BoolLiteral Bool
  | UnitLiteral
  deriving (Show)

data MatchCase = MatchCase {matchedConstructor :: String, matchedBinding :: String, caseBody :: Exp} deriving (Show)

data TypeRef
  = Int
  | Float
  | Bool
  | Unit
  | TypePtr TypeRef
  | TypeRef String
  deriving (Eq, Show)

data FunParam = FunParam {paramName :: String, paramType :: TypeRef} deriving (Show)

data StructField = StructField {fieldName :: String, fieldType :: TypeRef} deriving (Show)

data UnionConstructor = UnionConstructor {constructorName :: String, constructorType :: TypeRef} deriving (Show)

data Definition
  = FunDef String [FunParam] TypeRef Exp
  | StructDef String [StructField]
  | UnionDef String [UnionConstructor]
  deriving (Show)

data Program = Program [Definition] Exp deriving (Show)
