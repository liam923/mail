{-# LANGUAGE QuasiQuotes #-}

module Checker (check, TypeError) where

import Ast
import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Interpolate (i)

type TypeError = String

type CheckResult a = Either TypeError a

type TypeEnv = Map String Type

type FunctionEnv = Map String ([TypeRef], TypeRef)

type VarEnv = Map String TypeRef

data Env = Env {tenv :: TypeEnv, fenv :: FunctionEnv, venv :: VarEnv}

check :: Program -> CheckResult ()
check (Program defs body) =
  let typeNames = collectTypeNames defs
   in do
        typeEnv <- checkTypeDefs typeNames defs
        functionEnv <- checkFunctionSignatures typeEnv defs
        checkFunctions typeEnv functionEnv defs
        let bodyEnv = Env {tenv = typeEnv, fenv = functionEnv, venv = Map.empty}
        checkAndExpect bodyEnv Int body

collectTypeNames :: [Definition] -> Set String
collectTypeNames [] = Set.empty
collectTypeNames (FunDef {} : rest) = collectTypeNames rest
collectTypeNames (StructDef name _ : rest) = Set.insert name $ collectTypeNames rest
collectTypeNames (UnionDef name _ : rest) = Set.insert name $ collectTypeNames rest

checkTypeRef :: Set String -> TypeRef -> CheckResult TypeRef
checkTypeRef _ Int = Right Int
checkTypeRef _ Float = Right Float
checkTypeRef _ Bool = Right Bool
checkTypeRef _ Unit = Right Unit
checkTypeRef typeNames (TypeRef ref) =
  if Set.member ref typeNames then Right (TypeRef ref) else Left [i|Undefined type #{ref}|]

checkTypeDefs :: Set String -> [Definition] -> CheckResult TypeEnv
checkTypeDefs _ [] = Right Map.empty
checkTypeDefs typeNames (FunDef {} : rest) = checkTypeDefs typeNames rest
checkTypeDefs typeNames (StructDef name fields : rest) =
  do
    restTypeEnv <- checkTypeDefs typeNames rest
    mapM_ (checkTypeRef typeNames . fieldType) fields
    () <- if Map.member name restTypeEnv then Left [i|Duplicate type name #{name}|] else Right ()
    return (Map.insert name (Struct fields) restTypeEnv)
checkTypeDefs typeNames (UnionDef name constructors : rest) =
  do
    mapM_ (checkTypeRef typeNames . constructorType) constructors
    restTypeEnv <- checkTypeDefs typeNames rest
    () <- if Map.member name restTypeEnv then Left [i|Duplicate type name #{name}|] else Right ()
    return (Map.insert name (Union constructors) restTypeEnv)

checkFunctionSignatures :: TypeEnv -> [Definition] -> CheckResult FunctionEnv
checkFunctionSignatures _ [] = Right Map.empty
checkFunctionSignatures typeEnv (StructDef {} : rest) = checkFunctionSignatures typeEnv rest
checkFunctionSignatures typeEnv (UnionDef {} : rest) = checkFunctionSignatures typeEnv rest
checkFunctionSignatures typeEnv (FunDef name params ret _ : rest) =
  let typeNames = Map.keysSet typeEnv
   in do
        paramTypes <- mapM (checkTypeRef typeNames . paramType) params
        retType <- checkTypeRef typeNames ret
        restFunctionEnv <- checkFunctionSignatures typeEnv rest
        () <- if Map.member name restFunctionEnv then Left [i|Duplicate function name #{name}|] else Right ()
        return (Map.insert name (paramTypes, retType) restFunctionEnv)

checkFunctions :: TypeEnv -> FunctionEnv -> [Definition] -> CheckResult ()
checkFunctions _ _ [] = Right ()
checkFunctions typeEnv funcitonEnv (UnionDef {} : rest) = checkFunctions typeEnv funcitonEnv rest
checkFunctions typeEnv funcitonEnv (StructDef {} : rest) = checkFunctions typeEnv funcitonEnv rest
checkFunctions typeEnv functionEnv (FunDef _ params retType body : rest) =
  let varEnv = Map.fromList . map (\p -> (paramName p, paramType p)) $ params
   in let env = Env {tenv = typeEnv, fenv = functionEnv, venv = varEnv}
       in do
            () <- checkAndExpect env retType body
            checkFunctions typeEnv functionEnv rest

checkType :: Env -> Exp -> CheckResult TypeRef
checkType env (Let name value body) =
  do
    valueType <- checkType env value
    let extendedEnv = env {venv = Map.insert name valueType $ venv env}
    checkType extendedEnv body
checkType env (VarRef name) =
  case Map.lookup name $ venv env of
    Just type' -> Right type'
    Nothing -> Left [i|Undefined variable #{name}|]
checkType env (Eseq expr rest) =
  do
    checkAndExpect env Unit expr
    checkType env rest
checkType env (Assign name value) =
  case Map.lookup name $ venv env of
    Just varType -> do
      checkAndExpect env varType value
      return Unit
    Nothing -> Left [i|Undefined variable #{name}|]
checkType env (Ite cond tBody fBody) =
  do
    checkAndExpect env Bool cond
    tBodyType <- checkType env tBody
    fBodyType <- checkType env fBody
    () <-
      if tBodyType == fBodyType
        then Right ()
        else Left [i|If branches have type disagreement; true branch has type #{tBodyType}, false branch has type #{fBodyType}|]
    return tBodyType
checkType env (Call funName args) =
  do
    (paramTypes, retType) <-
      ( case Map.lookup funName $ fenv env of
          Just fType -> Right fType
          Nothing -> Left [i|Undefined function #{funName}|]
        )
    let expectedArgNum = length paramTypes
    let actualArgNum = length args
    () <- (if expectedArgNum == actualArgNum then Right () else Left [i|Expected #{expectedArgNum} arguments, got #{actualArgNum}|])
    () <- mapM_ (uncurry (checkAndExpect env)) $ List.zip paramTypes args
    return retType
checkType env (StructDeref struct deref) =
  do
    structType <- checkType env struct
    let notStructError = [i|Expected a struct type, got #{structType}|]
    structFields <- case structType of
      Int -> Left notStructError
      Bool -> Left notStructError
      Float -> Left notStructError
      Unit -> Left notStructError
      TypeRef ref -> case Map.lookup ref $ tenv env of
        Just (Struct fields) -> Right fields
        Just (Union _) -> Left notStructError
        Nothing -> Left [i|Undefined type #{ref}|]
    ( case find (\f -> fieldName f == deref) structFields of
        Just field -> Right (fieldType field)
        Nothing -> Left [i|No field with name #{deref}|]
      )
checkType env (Match value cases) =
  do
    valueType <- checkType env value
    let notUnionError = [i|Expected a union type, got #{valueType}|]
    unionConstructors <- case valueType of
      Int -> Left notUnionError
      Bool -> Left notUnionError
      Float -> Left notUnionError
      Unit -> Left notUnionError
      TypeRef ref -> case Map.lookup ref $ tenv env of
        Just (Union constructors) -> Right constructors
        Just (Struct _) -> Left notUnionError
        Nothing -> Left [i|Undefined type #{ref}|]
    () <- -- Check that the match is exhaustive
      mapM_
        ( \constructor ->
            let name = constructorName constructor
             in if cases
                  & find
                    ( \case' ->
                        matchedConstructor case' == name
                    )
                  & isJust
                  then Right ()
                  else Left [i|Unexhaustive match; #{name} unmatched|]
        )
        unionConstructors
    caseTypes <-
      cases
        & mapM
          ( \(MatchCase matchName binding body) ->
              case find (\c -> constructorName c == matchName) unionConstructors of
                Just (UnionConstructor _ matchedType) ->
                  let extendedEnv = env {venv = Map.insert binding matchedType $ venv env}
                   in checkType extendedEnv body
                Nothing -> Left [i|Unexpected constructor #{matchName}|]
          )
    case caseTypes of
      [] -> Left "Empty match statement"
      (headType : restTypes) ->
        if all (== headType) restTypes then Right headType else Left [i|Match branch disagreement|]
checkType _ (IntLiteral _) = return Int
checkType _ (FloatLiteral _) = return Float
checkType _ (BoolLiteral _) = return Bool
checkType env (UniOp NegateInt arg) =
  do
    checkAndExpect env Int arg
    return Int
checkType env (UniOp NegateBool arg) =
  do
    checkAndExpect env Bool arg
    return Bool
checkType env (BinOp op arg1 arg2) =
  let opType = binopType op
   in do
        checkAndExpect env opType arg1
        checkAndExpect env opType arg2
        return opType

checkAndExpect :: Env -> TypeRef -> Exp -> Either TypeError ()
checkAndExpect env expected expr =
  do
    type' <- checkType env expr
    assertTypeEqual expected type'

assertTypeEqual :: TypeRef -> TypeRef -> CheckResult ()
assertTypeEqual expected actual =
  if expected == actual then Right () else Left [i|Expected type #{expected}, got #{actual}|]

binopType :: BinOp -> TypeRef
binopType IntPlus = Int
binopType IntMinus = Int
binopType IntMul = Int
binopType IntDiv = Int
binopType IntEQ = Int
binopType IntNE = Int
binopType IntLT = Int
binopType IntLE = Int
binopType IntGT = Int
binopType IntGE = Int
binopType FloatPlus = Float
binopType FloatMinus = Float
binopType FloatMul = Float
binopType FloatDiv = Float
binopType FloatEQ = Float
binopType FloatNE = Float
binopType FloatLT = Float
binopType FloatLE = Float
binopType FloatGT = Float
binopType FloatGE = Float
binopType And = Bool
binopType Or = Bool
