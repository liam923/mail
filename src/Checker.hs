{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Checker (check, TypeError) where

import qualified Ast as U
import Checked (Identifier)
import qualified Checked as T
import Control.Monad.State
import Data.Foldable (find, foldl')
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Interpolate (i)
import Stdlib

type TypeError = String

type CheckResult a = Either TypeError a

type CheckState a = StateT (Map String Integer) (Either TypeError) a

type FunSignature = ([T.FunParam], T.TypeRef)

data Signature
  = NewStructSignature T.Identifier [T.StructField]
  | StructDerefSignature T.Identifier T.StructField Integer
  | UnionConstructorSignature T.Identifier T.UnionConstructor Integer
  | FunSignature T.Identifier FunSignature

type TypeEnv = Map Identifier T.Type

type FunctionEnv = Map String Signature

type LocalEnv = Map String (T.Identifier, T.TypeRef)

data Env = Env {tenv :: TypeEnv, fenv :: FunctionEnv, lenv :: LocalEnv}

getId :: String -> CheckState T.Identifier
getId str = do
  counts <- get
  let counter = Map.lookup str counts & fromMaybe 0
  let genedId = T.Identifier str counter
  put (Map.insert str (counter + 1) counts)
  pure genedId

check :: U.Program -> CheckResult T.Program
check (U.Program defs body) =
  runStateT stateProgram Map.empty >>= \(a, _) -> pure a
  where
    stateProgram = do
      (functionEnv, typeEnv, funs) <- checkSignatures defs
      functions <- checkFunctions typeEnv functionEnv funs
      checkedBody <- checkAndExpect (Env {tenv = typeEnv, fenv = functionEnv, lenv = Map.empty}) T.Int body
      pure $ T.Program typeEnv functions checkedBody

checkTypeRef :: Map String Identifier -> U.TypeRef -> CheckState (Set String, Map String Identifier, T.TypeRef)
checkTypeRef _ U.Int = pure (Set.empty, Map.empty, T.Int)
checkTypeRef _ U.Float = pure (Set.empty, Map.empty, T.Float)
checkTypeRef _ U.Bool = pure (Set.empty, Map.empty, T.Bool)
checkTypeRef _ U.Unit = pure (Set.empty, Map.empty, T.Unit)
checkTypeRef ids (U.TypeRef ref) = case Map.lookup ref ids of
  Just refId -> pure (Set.singleton ref, ids, T.TypeRef refId)
  Nothing -> getId ref >>= \refId -> pure (Set.singleton ref, Map.insert ref refId ids, T.TypeRef refId)

findDuplicate :: (Ord a) => [a] -> Maybe a
findDuplicate = loop Set.empty
  where
    loop _ [] = Nothing
    loop seen (h : rest) = if Set.member h seen then Just h else loop (Set.insert h seen) rest

mapi :: (Num x) => (x -> a -> b) -> [a] -> [b]
mapi f l = loop l [] 0
  where
    loop [] acc _ = acc
    loop (x : xs) acc ix = loop xs (f ix x : acc) (ix + 1)

checkSignatures :: [U.Definition] -> CheckState (FunctionEnv, TypeEnv, [(Identifier, FunSignature, U.Exp)])
checkSignatures = loop (Map.keysSet stdlib) Set.empty Map.empty
  where
    loop :: Set String -> Set String -> Map String Identifier -> [U.Definition] -> CheckState (FunctionEnv, TypeEnv, [(Identifier, FunSignature, U.Exp)])
    loop _ needs _ [] = case Set.toList needs of
      [] -> pure (Map.empty, Map.empty, [])
      (name : _) -> lift $ Left [i|Undefined type #{name}|]
    loop seen needs ids (U.FunDef funName params ret funBody : rest) = do
      funId <- (if Set.member funName seen then lift $ Left [i|Duplicate name for function #{funName}|] else getId funName)
      () <-
        ( case params & map U.paramName & findDuplicate of
            Just paramName -> lift $ Left [i|Duplicate param #{paramName}|]
            Nothing -> pure ()
          )
      (paramNeeds, idsAfterParams, checkedParamsRev) <-
        foldl'
          ( \acc (U.FunParam paramName type') -> do
              paramId <- getId paramName
              (needsSoFar, idsSoFar, paramsSoFar) <- acc
              (paramNeeds, newIds, checkedType) <- checkTypeRef idsSoFar type'
              pure (Set.union needsSoFar paramNeeds, newIds, T.FunParam paramId checkedType : paramsSoFar)
          )
          (pure (Set.empty, ids, []))
          params
      let checkedParams = reverse checkedParamsRev
      (retNeeds, idsAfterRet, checkedRet) <- checkTypeRef idsAfterParams ret
      let newNeeds = Set.unions [needs, paramNeeds, retNeeds]
      let signature = (checkedParams, checkedRet)
      (restEnv, restTypes, restFuns) <- loop (Set.insert funName seen) newNeeds idsAfterRet rest
      pure
        ( Map.insert funName (FunSignature funId signature) restEnv,
          restTypes,
          (funId, signature, funBody) : restFuns
        )
    loop seen needs ids (U.StructDef structName fields : rest) =
      do
        structId <- (if Set.member structName seen then lift $ Left [i|Duplicate name for struct #{structName}|] else getId structName)
        () <-
          ( case fields & map U.fieldName & findDuplicate of
              Just fieldName -> lift $ Left [i|Duplicate fields #{fieldName}|]
              Nothing -> pure ()
            )
        (seenAfterFields, fieldNeeds, idsAfterFields, checkedFieldsRev) <-
          foldl'
            ( \acc (U.StructField fieldName type') -> do
                (seenSoFar, needsSoFar, idsSoFar, fieldsSoFar) <- acc
                (if Set.member fieldName seenSoFar then lift $ Left [i|Duplicate name for struct field #{fieldName}|] else pure ())
                (fieldNeeds, newIds, checkedType) <- checkTypeRef idsSoFar type'
                pure (Set.insert fieldName seenSoFar, Set.union needsSoFar fieldNeeds, newIds, T.StructField fieldName checkedType : fieldsSoFar)
            )
            (pure (Set.insert structName seen, Set.empty, ids, []))
            fields
        let checkedFields = reverse checkedFieldsRev
        let newNeeds = Set.union needs fieldNeeds
        (restEnv, restTypes, restFuns) <- loop seenAfterFields newNeeds idsAfterFields rest
        let derefSignatures = mapi (\ix f -> (T.fieldName f, StructDerefSignature structId f ix)) checkedFields
        let newStructSignature = (structName, NewStructSignature structId checkedFields)
        pure
          ( foldl' (\e (n, s) -> Map.insert n s e) restEnv (newStructSignature : derefSignatures),
            Map.insert structId (T.Struct checkedFields) restTypes,
            restFuns
          )
    loop seen needs ids (U.UnionDef unionName constructors : rest) =
      do
        unionId <- (if Set.member unionName seen then lift $ Left [i|Duplicate name for union #{unionName}|] else getId unionName)
        () <-
          ( case constructors & map U.constructorName & findDuplicate of
              Just constructorName -> lift $ Left [i|Duplicate constructors #{constructorName}|]
              Nothing -> pure ()
            )
        (seenAfterConstructors, constructorNeeds, idsAfterConstructors, checkedConstructorsRev) <-
          foldl'
            ( \acc (U.UnionConstructor constructorName type') -> do
                (seenSoFar, needsSoFar, idsSoFar, constructorsSoFar) <- acc
                (if Set.member constructorName seenSoFar then lift $ Left [i|Duplicate name for union constructor #{constructorName}|] else pure ())
                (constructorNeeds, newIds, checkedType) <- checkTypeRef idsSoFar type'
                pure
                  ( Set.insert constructorName seenSoFar,
                    Set.union needsSoFar constructorNeeds,
                    newIds,
                    T.UnionConstructor constructorName checkedType : constructorsSoFar
                  )
            )
            (pure (Set.insert unionName seen, Set.empty, ids, []))
            constructors
        let checkedConstructors = reverse checkedConstructorsRev
        let newNeeds = Set.union needs constructorNeeds
        (restEnv, restTypes, restFuns) <- loop seenAfterConstructors newNeeds idsAfterConstructors rest
        let constructorSignatures = mapi (\ix c -> (T.constructorName c, UnionConstructorSignature unionId c ix)) checkedConstructors
        pure
          ( foldl' (\e (n, s) -> Map.insert n s e) restEnv constructorSignatures,
            Map.insert unionId (T.Union checkedConstructors) restTypes,
            restFuns
          )

checkFunctions :: TypeEnv -> FunctionEnv -> [(Identifier, FunSignature, U.Exp)] -> CheckState (Map Identifier T.Fun)
checkFunctions typeEnv functionEnv funs = mapM checkFun funs <&> Map.fromList
  where
    checkFun (funId, (params, ret), body) = do
      let localEnv =
            params
              & map (\(T.FunParam paramId@(T.Identifier paramName _) paramType) -> (paramName, (paramId, paramType)))
              & Map.fromList
      let env = Env typeEnv functionEnv localEnv
      checkedBody <- checkAndExpect env ret body
      pure (funId, T.Fun params ret checkedBody)

checkType :: Env -> U.Exp -> CheckState T.Exp
checkType env (U.Let varName value body) =
  do
    varId <- getId varName
    checkedValue <- checkType env value
    let extendedEnv = env {lenv = Map.insert varName (varId, T.type' checkedValue) $ lenv env}
    checkedBody <- checkType extendedEnv body
    pure $ T.WithType (T.Let varId checkedValue checkedBody) (T.type' checkedBody)
checkType env (U.VarRef name) =
  case Map.lookup name $ lenv env of
    Just (varId, varType) -> pure $ T.WithType (T.VarRef varId) varType
    Nothing -> lift $ Left [i|Undefined variable #{name}|]
checkType env (U.Eseq expr body) =
  do
    checkedExpr <- checkAndExpect env T.Unit expr
    checkedBody <- checkType env body
    pure $ T.WithType (T.Eseq checkedExpr checkedBody) (T.type' checkedBody)
checkType env (U.Assign name value) =
  case Map.lookup name $ lenv env of
    Just (varId, varType) -> do
      checkedValue <- checkAndExpect env varType value
      pure $ T.WithType (T.Assign varId checkedValue) T.Unit
    Nothing -> lift $ Left [i|Undefined variable #{name}|]
checkType env (U.Ite cond tBody fBody) =
  do
    checkedCond <- checkAndExpect env T.Bool cond
    checkedTBody <- checkType env tBody
    checkedFBody <- checkType env fBody
    let tBodyType = T.type' checkedTBody
        fBodyType = T.type' checkedFBody
    () <-
      if tBodyType == fBodyType
        then pure ()
        else lift $ Left [i|If branches have type disagreement; true branch has type #{tBodyType}, false branch has type #{fBodyType}|]
    pure $ T.WithType (T.Ite checkedCond checkedTBody checkedFBody) tBodyType
checkType env (U.Call funName args) = case Map.lookup funName stdlib of
  Just (BinOpEntry binOp arg1Type arg2Type retType) -> do
    (arg1, arg2) <-
      ( case args of
          [arg1, arg2] -> pure (arg1, arg2)
          _ -> lift $ Left [i|Expected 2 args for function #{funName}, got #{length args}|]
        )
    checkedArg1 <- checkAndExpect env arg1Type arg1
    checkedArg2 <- checkAndExpect env arg2Type arg2
    pure $ T.WithType (T.BinOp binOp checkedArg1 checkedArg2) retType
  Just (UniOpEntry uniOp argType retType) -> do
    arg <-
      ( case args of
          [arg] -> pure arg
          _ -> lift $ Left [i|Expected 1 arg for function #{funName}, got #{length args}|]
        )
    checkedArg <- checkAndExpect env argType arg
    pure $ T.WithType (T.UniOp uniOp checkedArg) retType
  Nothing ->
    ( case Map.lookup funName $ fenv env of
        Just (FunSignature funId (params, retType)) -> do
          let expectedArgNum = length params
          let actualArgNum = length args
          () <- (if expectedArgNum == actualArgNum then pure () else lift $ Left [i|Expected #{expectedArgNum} arguments to #{funName}, got #{actualArgNum}|])
          checkedArgs <- mapM (\(T.FunParam _ paramType, arg) -> checkAndExpect env paramType arg) $ List.zip params args
          pure $ T.WithType (T.FunCall funId checkedArgs) retType
        Just (NewStructSignature structId fields) -> do
          let expectedArgNum = length fields
          let actualArgNum = length args
          () <- (if expectedArgNum == actualArgNum then pure () else lift $ Left [i|Expected #{expectedArgNum} args to construct #{funName}, got #{actualArgNum}|])
          checkedArgs <- mapM (\(T.StructField _ fieldType, arg) -> checkAndExpect env fieldType arg) $ List.zip fields args
          pure $ T.WithType (T.StructMake structId checkedArgs) (T.TypeRef structId)
        Just (StructDerefSignature structId field ix) -> do
          arg <-
            ( case args of
                [arg] -> pure arg
                _ -> lift $ Left [i|Expected 1 argument to #{funName}, got #{length args}|]
              )
          checkedArg <- checkAndExpect env (T.TypeRef structId) arg
          pure $ T.WithType (T.StructDeref checkedArg ix) (T.fieldType field)
        Just (UnionConstructorSignature unionId constructor ix) -> do
          arg <-
            ( case args of
                [arg] -> pure arg
                _ -> lift $ Left [i|Expected 1 argument to #{funName}, got #{length args}|]
              )
          checkedArg <- checkAndExpect env (T.TypeRef unionId) arg
          pure $ T.WithType (T.UnionMake unionId ix checkedArg) (T.constructorType constructor)
        Nothing -> lift $ Left [i|Undefined function #{funName}|]
    )
checkType env (U.Match value cases) =
  do
    checkedValue <- checkType env value
    let notUnionError = [i|Expected a union type, got #{T.type' checkedValue}|]
    unionConstructors <- case T.type' checkedValue of
      T.Int -> lift $ Left notUnionError
      T.Bool -> lift $ Left notUnionError
      T.Float -> lift $ Left notUnionError
      T.Unit -> lift $ Left notUnionError
      T.TypeRef ref -> case Map.lookup ref $ tenv env of
        Just (T.Union constructors) -> pure constructors
        Just (T.Struct _) -> lift $ Left notUnionError
        Nothing -> lift $ Left [i|Undefined type #{ref}|]
    -- check for exhaustiveness
    mapM_
      ( \constructor ->
          let name = T.constructorName constructor
           in if cases
                & find
                  ( \case' ->
                      U.matchedConstructor case' == name
                  )
                & isJust
                then pure ()
                else lift $ Left [i|Unexhaustive match; #{name} unmatched|]
      )
      unionConstructors
    -- check that the same case isn't matched twice
    ( case cases & map U.matchedConstructor & findDuplicate of
        Just duplicate -> lift $ Left [i|Duplicate match of constructor #{duplicate}|]
        Nothing -> pure ()
      )
    checkedCases <-
      mapM
        ( \(U.MatchCase matchName binding body) ->
            case unionConstructors & mapi (,) & find (\(_, c) -> T.constructorName c == matchName) of
              Just (ix, T.UnionConstructor _ matchedType) -> do
                varId <- getId binding
                let extendedEnv = env {lenv = Map.insert binding (varId, matchedType) $ lenv env}
                checkedBody <- checkType extendedEnv body
                pure $ T.MatchCase ix checkedBody
              Nothing -> lift $ Left [i|Unexpected constructor #{matchName}|]
        )
        cases
    T.WithType (T.Match checkedValue checkedCases)
      <$> ( case map (T.type' . T.caseBody) checkedCases of
              [] -> lift $ Left "Empty match statement"
              (headType : restTypes) ->
                if all (== headType) restTypes then pure headType else lift $ Left [i|Match branch disagreement|]
          )
checkType _ (U.IntLiteral v) = pure $ T.WithType (T.IntLiteral v) T.Int
checkType _ (U.FloatLiteral f) = pure $ T.WithType (T.FloatLiteral f) T.Float
checkType _ (U.BoolLiteral b) = pure $ T.WithType (T.BoolLiteral b) T.Bool
checkType _ U.UnitLiteral = pure $ T.WithType T.UnitLiteral T.Unit

checkAndExpect :: Env -> T.TypeRef -> U.Exp -> CheckState T.Exp
checkAndExpect env expected expr =
  do
    typedExpr <- checkType env expr
    lift $ assertTypeEqual expected (T.type' typedExpr)
    pure typedExpr

assertTypeEqual :: T.TypeRef -> T.TypeRef -> CheckResult ()
assertTypeEqual expected actual =
  if expected == actual then Right () else Left [i|Expected type #{expected}, got #{actual}|]
