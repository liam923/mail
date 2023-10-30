{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Generator (gen) where

import qualified Checked as M
import Control.Monad.Fix (MonadFix)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Map (Map, mapMaybe)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.String.Interpolate (i)
import Debug.Trace (trace, traceShowId, traceShowM)
import LLVM.AST (Module, Operand (ConstantOperand), mkName)
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.FloatingPointPredicate as FloatingPointPredicate
import qualified LLVM.AST.IntegerPredicate as IntegerPredicate
import LLVM.AST.Type as T
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import qualified LLVM.IRBuilder.Instruction as I
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

data TypeEnvEntry = TypeEnvEntry {boxedType :: Type, rawType :: Type} deriving (Show)

type TypeEnv = Map M.Identifier TypeEnvEntry

type OperandEnv = Map M.Identifier Operand

data Env = Env
  { tenv :: TypeEnv,
    oenv :: OperandEnv,
    mallocRef :: Operand,
    freeRef :: Operand,
    typeDefs :: Map M.Identifier M.Type,
    unionValueTypes :: Map M.Identifier Type
  }
  deriving (Show)

genTypeBoxed :: Env -> M.TypeRef -> Type
genTypeBoxed _ M.Int = T.i64
genTypeBoxed _ M.Float = T.double
genTypeBoxed _ M.Bool = T.i1
genTypeBoxed _ M.Unit = T.i1
genTypeBoxed env (M.TypePtr t) = T.ptr (genTypeBoxed env t)
genTypeBoxed env (M.TypeRef ref) = boxedType $ fromJust $ Map.lookup ref (tenv env)

genTypeRaw :: Env -> M.TypeRef -> Type
genTypeRaw _ M.Int = T.i64
genTypeRaw _ M.Float = T.double
genTypeRaw _ M.Bool = T.i1
genTypeRaw _ M.Unit = T.i1
genTypeRaw env (M.TypePtr t) = T.ptr (genTypeBoxed env t)
genTypeRaw env (M.TypeRef ref) = rawType $ fromJust $ Map.lookup ref (tenv env)

boxedTypeFromRaw :: M.Type -> Type -> Type
boxedTypeFromRaw (M.Struct _) raw = T.ptr raw
boxedTypeFromRaw (M.Union _) raw = T.ptr raw

genTypeDef :: (MonadModuleBuilder m) => Env -> M.Identifier -> M.Type -> m Type
genTypeDef env (M.Identifier name _) (M.Struct fields) =
  let fieldTypes = map (genTypeRaw env . M.fieldType) fields
   in typedef (mkName name) (Just $ T.StructureType {T.isPacked = False, T.elementTypes = fieldTypes})
genTypeDef env (M.Identifier name _) (M.Union constructors) =
  let valueType = unionToUnionValueType env constructors
      tagType = T.i8
   in typedef (mkName name) (Just $ T.StructureType {T.isPacked = False, T.elementTypes = [tagType, valueType]})

typeToUnionValueType :: Env -> M.Type -> Maybe Type
typeToUnionValueType _ (M.Struct _) = Nothing
typeToUnionValueType env (M.Union constructors) = Just $ unionToUnionValueType env constructors

unionToUnionValueType :: Env -> [M.UnionConstructor] -> Type
unionToUnionValueType env constructors =
  let size = maximum $ map (sizeofRaw env . M.constructorType) constructors
   in T.ArrayType size T.i8

sizeofRaw :: (Num a, Ord a) => Env -> M.TypeRef -> a
sizeofRaw _ M.Int = 8
sizeofRaw _ M.Float = 8
sizeofRaw _ M.Bool = 1
sizeofRaw _ M.Unit = 1
sizeofRaw _ (M.TypePtr _) = 8
sizeofRaw env (M.TypeRef ref) =
  let refType = fromJust $ Map.lookup ref $ typeDefs env
   in case refType of
        M.Struct fields -> sum $ map (sizeofRaw env . M.fieldType) fields
        M.Union constructors ->
          let valueSize = maximum $ map (sizeofRaw env . M.constructorType) constructors
           in valueSize + 1

isBoxed :: Env -> M.TypeRef -> Bool
isBoxed _ M.Int = False
isBoxed _ M.Float = False
isBoxed _ M.Bool = False
isBoxed _ M.Unit = False
isBoxed _ (M.TypePtr _) = False
isBoxed env (M.TypeRef ref) = case fromJust $ Map.lookup ref (typeDefs env) of
  M.Struct _ -> True
  M.Union _ -> True

copyValue :: (MonadModuleBuilder m, MonadIRBuilder m) => Env -> M.TypeRef -> Operand -> m Operand
copyValue _ M.Int value = pure value
copyValue _ M.Float value = pure value
copyValue _ M.Bool value = pure value
copyValue _ M.Unit value = pure value
copyValue _ (M.TypePtr _) value = pure value
copyValue env type'@(M.TypeRef _) value = do
  addr <- alloca (genTypeRaw env type') Nothing 0
  copyValueToRawAddr env type' value addr
  pure addr

copyValueToRawAddr :: (MonadModuleBuilder m, MonadIRBuilder m) => Env -> M.TypeRef -> Operand -> Operand -> m ()
copyValueToRawAddr _ M.Int value addr = store addr 0 value
copyValueToRawAddr _ M.Float value addr = store addr 0 value
copyValueToRawAddr _ M.Bool value addr = store addr 0 value
copyValueToRawAddr _ M.Unit value addr = store addr 0 value
copyValueToRawAddr _ (M.TypePtr _) value addr = store addr 0 value
copyValueToRawAddr env (M.TypeRef ref) value addr = case fromJust $ Map.lookup ref (typeDefs env) of
  M.Struct fields ->
    fields
      & zip [0 ..]
      & mapM_
        ( \(ix, field) -> do
            let fieldType = M.fieldType field
            source <- gep value [C.int32 0, C.int32 ix]
            fieldValue <- (if isBoxed env fieldType then pure source else load source 0)
            dest <- gep addr [C.int32 0, C.int32 ix]
            copyValueToRawAddr env fieldType fieldValue dest
        )
  M.Union constructors -> error "poop"


copyValueToBoxedAddr :: (MonadModuleBuilder m, MonadIRBuilder m) => Env -> M.TypeRef -> Operand -> Operand -> m ()
copyValueToBoxedAddr _ M.Int value addr = store addr 0 value
copyValueToBoxedAddr _ M.Float value addr = store addr 0 value
copyValueToBoxedAddr _ M.Bool value addr = store addr 0 value
copyValueToBoxedAddr _ M.Unit value addr = store addr 0 value
copyValueToBoxedAddr _ (M.TypePtr _) value addr = store addr 0 value
copyValueToBoxedAddr env type'@(M.TypeRef ref) value addr = case fromJust $ Map.lookup ref (typeDefs env) of
  M.Struct _ -> do
    dest <- load addr 0
    copyValueToRawAddr env type' value dest
  M.Union _ -> do
    dest <- load addr 0
    copyValueToRawAddr env type' value dest

genFunDef :: (MonadModuleBuilder m, MonadFix m) => Env -> M.Identifier -> M.Fun -> m Operand
genFunDef env funId (M.Fun params retType body) = do
  let returnThroughPointerArg = isBoxed env retType
  let paramsLRaw =
        map
          ( \(M.FunParam name type') ->
              (genTypeBoxed env type', fromString $ M.idName name)
          )
          params
  let retTypeLRaw = genTypeBoxed env retType
  let (paramsL, retTypeL) =
        if returnThroughPointerArg
          then ((retTypeLRaw, "return") : paramsLRaw, T.VoidType)
          else (paramsLRaw, retTypeLRaw)
  function (mkName (M.idName funId)) paramsL retTypeL $ \argsLRaw -> do
    _ <- block `named` "entry"
    let (returnResult, argsL) =
          ( if returnThroughPointerArg
              then (\result -> copyValueToRawAddr env retType result (head argsLRaw), tail argsLRaw)
              else (ret, argsLRaw)
          )
    argEnvEntries <-
      zip params argsL
        & mapM
          ( \(M.FunParam paramName paramType, argL) ->
              if isBoxed env paramType
                then do
                  addr <- alloca (genTypeRaw env paramType) Nothing 0
                  copyValueToRawAddr env paramType argL addr
                  pure (paramName, addr)
                else do
                  addr <- alloca (genTypeBoxed env paramType) Nothing 0
                  copyValueToBoxedAddr env paramType argL addr
                  pure (paramName, addr)
          )
    let extendedEnv = env {oenv = oenv env <> Map.fromList argEnvEntries}
    bodyL <- genExp extendedEnv body
    returnResult bodyL

genExp :: (MonadModuleBuilder m, MonadIRBuilder m, MonadFix m) => Env -> M.Exp -> m Operand
genExp env (M.WithType expr type') = case expr of
  M.Let binding value body -> do
    valueL <- genExp env value
    addr <-
      ( if isBoxed env $ M.type' value
          then pure valueL
          else do
            addr <- alloca (genTypeRaw env (M.type' value)) Nothing 0
            store addr 0 valueL
            pure addr
        )
    let extendedEnv = env {oenv = Map.insert binding addr $ oenv env}
    genExp extendedEnv body
  M.VarRef ref -> do
    let addr = fromJust (Map.lookup ref $ oenv env)
    (if isBoxed env type' then pure addr else load addr 0)
  M.Eseq first rest -> genExp env first *> genExp env rest
  M.Assign binding value -> do
    let addr = fromJust (Map.lookup binding $ oenv env)
    valueL <- genExp env value
    store addr 0 valueL
    pure $ C.int8 0
  M.Ite cond tbody fbody -> mdo
    condL <- genExp env cond
    condBr condL thenBlock elseBlock
    thenBlock <- block `named` "if.then"
    tbodyL <- genExp env tbody
    br thenRetBlock
    thenRetBlock <- block `named` "if.then.ret"
    br exitBlock
    elseBlock <- block `named` "if.else"
    fbodyL <- genExp env fbody
    br elseRetBlock
    elseRetBlock <- block `named` "if.else.ret"
    br exitBlock
    exitBlock <- block `named` "if.exit"
    phi [(tbodyL, thenRetBlock), (fbodyL, elseRetBlock)]
  M.While cond body -> mdo
    br condBlock
    condBlock <- block `named` "while.cond"
    condL <- genExp env cond
    condBr condL bodyBlock exitBlock
    bodyBlock <- block `named` "while.body"
    _ <- genExp env body
    br condBlock
    exitBlock <- block `named` "while.exit"
    genExp env (M.WithType M.UnitLiteral M.Unit)
  M.FunCall funId args ->
    let funAddr = fromJust $ Map.lookup funId $ oenv env
        returnThroughPointerArg = isBoxed env type'
     in ( if returnThroughPointerArg
            then do
              argsLRaw <- mapM (genExp env) args
              resultAddr <- alloca (genTypeRaw env type') Nothing 0
              let argsL = resultAddr : argsLRaw
              let attributedArgs = map (\arg -> (arg, [])) argsL
              _ <- call funAddr attributedArgs
              pure resultAddr
            else do
              argsL <- mapM (genExp env) args
              let attributedArgs = map (\arg -> (arg, [])) argsL
              res <- call funAddr attributedArgs
              copyValue env type' res
        )
  M.StructMake structId args -> do
    addr <- alloca (rawType $ fromJust $ Map.lookup structId (tenv env)) Nothing 0
    args
      & zip [0 ..]
      & mapM_
        ( \(ix, arg) -> do
            argL <- genExp env arg
            fieldAddr <- gep addr [C.int32 0, C.int32 ix]
            copyValueToRawAddr env (M.type' arg) argL fieldAddr
        )
    pure addr
  M.StructDeref obj ix -> do
    objL <- genExp env obj
    addr <- gep objL [C.int32 0, C.int32 $ fromIntegral ix]
    (if isBoxed env type' then pure addr else load addr 0)
  M.UnionMake unionId tag value -> error "poop"
  M.Match _ _ -> error "poop"
  M.Alloc value -> do
    let valueType = M.type' value
    let boxedTypeL = genTypeRaw env valueType
    valueL <- genExp env value
    rawAddr <- call (mallocRef env) [(C.int64 $ sizeofRaw env valueType, [])]
    addr <- bitcast rawAddr $ ptr boxedTypeL
    copyValueToRawAddr env valueType valueL addr
    pure addr
  M.Dealloc pointer -> do
    pointerL <- genExp env pointer
    _ <- call (freeRef env) [(pointerL, [])]
    genExp env (M.WithType M.UnitLiteral M.Unit)
  M.SetPointer pointer value -> do
    pointerL <- genExp env pointer
    valueL <- genExp env value
    store pointerL 0 valueL
    pure $ C.int8 0
  M.GetPointer pointer -> do
    pointerL <- genExp env pointer
    load pointerL 0
  M.IntLiteral v -> pure $ C.int64 v
  M.FloatLiteral f -> pure $ C.double f
  M.BoolLiteral b -> pure $ C.int8 (if b then 1 else 0)
  M.UnitLiteral -> pure $ C.int8 0
  M.UniOp op arg -> do
    argL <- genExp env arg
    let f =
          ( case op of
              M.NegateInt -> sub (C.int64 0)
              M.NegateFloat -> fsub (C.double 0)
              M.NegateBool -> icmp IntegerPredicate.EQ (C.int8 0)
          )
    f argL
  M.BinOp op arg1 arg2 -> do
    arg1L <- genExp env arg1
    arg2L <- genExp env arg2
    let f =
          ( case op of
              M.IntPlus -> add
              M.IntMinus -> sub
              M.IntMul -> mul
              M.IntDiv -> sdiv
              M.IntLT -> icmp IntegerPredicate.SLT
              M.IntLE -> icmp IntegerPredicate.SLE
              M.IntGT -> icmp IntegerPredicate.SGT
              M.IntGE -> icmp IntegerPredicate.SGE
              M.IntEQ -> icmp IntegerPredicate.EQ
              M.IntNE -> icmp IntegerPredicate.NE
              M.FloatPlus -> fadd
              M.FloatMinus -> fsub
              M.FloatMul -> fmul
              M.FloatDiv -> fdiv
              M.FloatLT -> fcmp FloatingPointPredicate.OLT
              M.FloatLE -> fcmp FloatingPointPredicate.OLE
              M.FloatGT -> fcmp FloatingPointPredicate.OGT
              M.FloatGE -> fcmp FloatingPointPredicate.OGE
              M.FloatEQ -> fcmp FloatingPointPredicate.OEQ
              M.FloatNE -> fcmp FloatingPointPredicate.ONE
              M.And -> I.and
              M.Or -> I.or
          )
    f arg1L arg2L

gen :: String -> M.Program -> Module
gen progName (M.Program types funs body) =
  let modl = buildModule (fromString progName) $ mdo
        printNum <- extern "printNum" [T.i64] T.VoidType
        malloc <- extern "malloc" [T.i64] (T.ptr T.i8)
        free <- externVarArgs "free" [] T.i32

        typeEnv <-
          sequence $
            Map.mapWithKey
              ( \typeId type' -> do
                  rawTypeRef <- genTypeDef env typeId type'
                  let boxedTypeRef = boxedTypeFromRaw type' rawTypeRef
                  pure $ TypeEnvEntry {boxedType = boxedTypeRef, rawType = rawTypeRef}
              )
              types
        functionEnv <- sequence $ Map.mapWithKey (genFunDef env) funs
        let unionValues = mapMaybe (typeToUnionValueType env) types
        let env = Env typeEnv functionEnv malloc free types unionValues

        function "main" [] T.i32 $ \_ -> do
          _ <- block `named` "entry"

          bodyRes <- genExp env body
          _ <- call printNum [(bodyRes, [])]
          ret $ C.int32 0
   in modl
