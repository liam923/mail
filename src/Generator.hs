{-# LANGUAGE OverloadedStrings #-}
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
import Debug.Trace (trace, traceShowId)
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

data TypeEnvEntry = TypeEnvEntry {firstClassType :: Type, anyType :: Type}

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

genTypeFirstClass :: Env -> M.TypeRef -> Type
genTypeFirstClass _ M.Int = T.i64
genTypeFirstClass _ M.Float = T.double
genTypeFirstClass _ M.Bool = T.i1
genTypeFirstClass _ M.Unit = T.i1
genTypeFirstClass env (M.TypePtr t) = T.ptr (genTypeFirstClass env t)
genTypeFirstClass env (M.TypeRef ref) = firstClassType $ fromJust $ Map.lookup ref (tenv env)

genTypeAny :: Env -> M.TypeRef -> Type
genTypeAny _ M.Int = T.i64
genTypeAny _ M.Float = T.double
genTypeAny _ M.Bool = T.i1
genTypeAny _ M.Unit = T.i1
genTypeAny env (M.TypePtr t) = T.ptr (genTypeFirstClass env t)
genTypeAny env (M.TypeRef ref) = anyType $ fromJust $ Map.lookup ref (tenv env)

genTypeDef :: (MonadModuleBuilder m) => Env -> M.Identifier -> M.Type -> m Type
genTypeDef env (M.Identifier name _) (M.Struct fields) =
  let fieldTypes = map (genTypeAny env . M.fieldType) fields
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
  let size = maximum $ map (sizeofAny env . M.constructorType) constructors
   in T.ArrayType size T.i8

isFirstClass :: Env -> M.TypeRef -> Bool
isFirstClass _ M.Int = True
isFirstClass _ M.Float = True
isFirstClass _ M.Bool = True
isFirstClass _ M.Unit = True
isFirstClass _ (M.TypePtr _) = True
isFirstClass env (M.TypeRef ref) = case fromJust $ Map.lookup ref (typeDefs env) of
  M.Struct _ -> False
  M.Union _ -> False

-- This function should be based on a DataLayout to account for machine specifics
sizeofFirstClass :: (Num p, Ord p) => Env -> M.TypeRef -> p
sizeofFirstClass _ M.Int = 8
sizeofFirstClass _ M.Float = 8
sizeofFirstClass _ M.Bool = 1
sizeofFirstClass _ M.Unit = 1
sizeofFirstClass _ (M.TypePtr _) = 8
sizeofFirstClass env (M.TypeRef ref) =
  let sizeofType (M.Struct fields) = sum $ map (sizeofAny env . M.fieldType) fields
      sizeofType (M.Union constructors) = let valueSize = maximum $ map (sizeofAny env . M.constructorType) constructors in valueSize + 1
   in let refType = fromJust $ Map.lookup ref $ typeDefs env in sizeofType refType

sizeofAny :: (Num p, Ord p) => Env -> M.TypeRef -> p
sizeofAny _ M.Int = 8
sizeofAny _ M.Float = 8
sizeofAny _ M.Bool = 1
sizeofAny _ M.Unit = 1
sizeofAny _ (M.TypePtr _) = 8
sizeofAny env (M.TypeRef ref) =
  let sizeofType (M.Struct fields) = sum $ map (sizeofAny env . M.fieldType) fields
      sizeofType (M.Union constructors) = let valueSize = maximum $ map (sizeofAny env . M.constructorType) constructors in valueSize + 1
   in let refType = fromJust $ Map.lookup ref $ typeDefs env in sizeofType refType

genFunDef :: (MonadModuleBuilder m, MonadFix m) => Env -> M.Identifier -> M.Fun -> m Operand
genFunDef env funId (M.Fun params retType body) = do
  let paramsL =
        map
          ( \(M.FunParam name type') ->
              (genTypeAny env type', fromString $ M.idName name)
          )
          params
  let retTypeL = genTypeAny env retType
  function (mkName (M.idName funId)) paramsL retTypeL $ \argsL -> do
    _ <- block `named` "entry"
    argEnvEntries <-
      zip params argsL
        & mapM
          ( \(param, argL) -> do
              addr <- alloca (genTypeAny env $ M.paramType param) Nothing 0
              store addr 0 argL
              pure (M.paramName param, addr)
          )
    let extendedEnv = env {oenv = oenv env <> Map.fromList argEnvEntries}
    bodyL <- genExp extendedEnv body
    ret bodyL

genExp :: (MonadModuleBuilder m, MonadIRBuilder m, MonadFix m) => Env -> M.Exp -> m Operand
genExp env (M.WithType expr type') = case expr of
  M.Let binding value body -> do
    valueL <- genExp env value
    addr <- alloca (genTypeFirstClass env (M.type' value)) Nothing 0
    store (trace "addr" (traceShowId addr)) 0 (trace "value" (traceShowId valueL))
    let extendedEnv = env {oenv = Map.insert binding addr $ oenv env}
    genExp extendedEnv body
  M.VarRef ref -> do
    let addr = fromJust (Map.lookup ref $ oenv env)
    load addr 0
  M.Eseq first rest -> genExp env first *> genExp env rest
  M.Assign binding value -> do
    let addr = fromJust (Map.lookup binding $ oenv env)
    valueL <- genExp env value
    store addr 0 valueL
    pure $ C.bit 0
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
  M.FunCall funId args -> do
    argsL <- mapM (genExp env) args
    let attributedArgs = map (\arg -> (arg, [])) argsL
    let funAddr = fromJust $ Map.lookup funId $ oenv env
    call funAddr attributedArgs
  M.StructMake structId args ->
    do
      let structType = firstClassType $ fromJust $ Map.lookup structId $ tenv env
      structPointer <- alloca (traceShowId structType) Nothing 0
      -- zip [0 ..] args
      --   & mapM_
      --     ( \(ix, arg) -> do
      --         addr <- gep structPointer [C.int32 0, C.int32 ix]
      --         argL <- genExp env arg
      --         store addr 0 argL
      --     )
      pure structPointer
  M.StructDeref obj ix -> do
    objL <- genExp env obj
    fieldPointer <- gep objL [C.int32 $ fromIntegral ix]
    (if isFirstClass env type' then load fieldPointer 0 else pure fieldPointer)
  M.UnionMake unionId tag value -> error "poop"
  -- do
  -- let unionType = fromJust $ Map.lookup unionId $ tenv env
  -- let unionValueType = fromJust $ Map.lookup unionId $ unionValueTypes env
  -- let tagL = C.int8 $ fromIntegral tag
  -- valueL <- genExp env value
  -- -- castedValue <- bitcast valueL unionValueType
  -- structWithTag <- insertValue (ConstantOperand $ Constant.Undef unionType) tagL [0]
  -- -- insertValue structWithTag castedValue [1]
  -- pure structWithTag
  M.Match _ _ -> error "poop"
  M.Alloc value -> do
    let boxedType = M.type' value
    let boxedTypeL = genTypeAny env boxedType
    rawAddr <- call (mallocRef env) [(C.int64 $ sizeofAny env boxedType, [])]
    addr <- bitcast rawAddr $ ptr boxedTypeL
    valueL <- genExp env value
    store addr 0 valueL
    pure addr
  M.Dealloc pointer -> do
    pointerL <- genExp env pointer
    _ <- call (freeRef env) [(pointerL, [])]
    genExp env (M.WithType M.UnitLiteral M.Unit)
  M.SetPointer pointer value -> do
    pointerL <- genExp env pointer
    valueL <- genExp env value
    store pointerL 0 valueL
    pure $ C.bit 0
  M.GetPointer pointer -> do
    pointerL <- genExp env pointer
    load pointerL 0
  M.IntLiteral v -> pure $ C.int64 v
  M.FloatLiteral f -> pure $ C.double f
  M.BoolLiteral b -> pure $ C.bit (if b then 1 else 0)
  M.UnitLiteral -> pure $ C.bit 0
  M.UniOp op arg -> do
    argL <- genExp env arg
    let f =
          ( case op of
              M.NegateInt -> sub (C.int64 0)
              M.NegateFloat -> fsub (C.double 0)
              M.NegateBool -> icmp IntegerPredicate.EQ (C.bit 0)
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

        typeEnvAny <- sequence $ Map.mapWithKey (genTypeDef env) types
        let typeEnv = Map.map (\t -> TypeEnvEntry {firstClassType = T.ptr t, anyType = t}) typeEnvAny
        functionEnv <- sequence $ Map.mapWithKey (genFunDef env) funs
        let unionValues = mapMaybe (typeToUnionValueType env) types
        let env = Env typeEnv functionEnv malloc free types unionValues

        function "main" [] T.i32 $ \_ -> do
          _ <- block `named` "entry"

          bodyRes <- genExp env body
          _ <- call printNum [(bodyRes, [])]
          ret $ C.int32 0
   in modl
