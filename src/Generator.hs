{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Generator (gen) where

import qualified Checked as M
import Control.Monad.Fix (MonadFix)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, mapMaybe)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.String (fromString)
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

type TypeEnv = Map M.Identifier Type

type OperandEnv = Map M.Identifier Operand

data Env = Env
  { tenv :: TypeEnv,
    oenv :: OperandEnv,
    mallocRef :: Operand,
    freeRef :: Operand,
    typeDefs :: Map M.Identifier M.Type,
    unionValueTypes :: Map M.Identifier Type
  }

alignment :: (Num a) => a
alignment = 8

genType :: Env -> M.TypeRef -> Type
genType _ M.Int = T.i64
genType _ M.Float = T.double
genType _ M.Bool = T.i1
genType _ M.Unit = T.i1
genType env (M.TypePtr t) = T.ptr (genType env t)
genType env (M.TypeRef ref) = fromJust $ Map.lookup ref (tenv env)

genTypeDef :: (MonadModuleBuilder m) => Env -> M.Identifier -> M.Type -> m Type
genTypeDef env (M.Identifier name _) (M.Struct fields) =
  let fieldTypes = map (genType env . M.fieldType) fields
   in typedef (mkName name) (Just $ T.StructureType {T.isPacked = False, T.elementTypes = fieldTypes})
genTypeDef env (M.Identifier name _) (M.Union constructors) =
  let valueType = unionToUnionValueType env constructors
      tagType = T.i8
   in typedef (mkName name) (Just $ T.StructureType {T.isPacked = False, T.elementTypes = [tagType, valueType]})

typeToUnionValueType :: Env -> M.Type -> Maybe Type
typeToUnionValueType _ (M.Struct _) = Nothing
typeToUnionValueType env (M.Union constructors) = Just $ unionToUnionValueType env constructors

unionToUnionValueType :: Env -> NonEmpty M.UnionConstructor -> Type
unionToUnionValueType env constructors =
  let size = maximum $ NonEmpty.map (sizeof env . M.constructorType) constructors
   in T.ArrayType size T.i8

-- This function should be based on a DataLayout to account for machine specifics
sizeof :: (Num p, Ord p) => Env -> M.TypeRef -> p
sizeof _ M.Int = 8
sizeof _ M.Float = 8
sizeof _ M.Bool = 1
sizeof _ M.Unit = 1
sizeof _ (M.TypePtr _) = 8
sizeof env (M.TypeRef ref) =
  let sizeofType (M.Struct fields) = sum $ map (sizeof env . M.fieldType) fields
      sizeofType (M.Union constructors) = let valueSize = maximum $ NonEmpty.map (sizeof env . M.constructorType) constructors in valueSize + 1
   in let refType = fromJust $ Map.lookup ref $ typeDefs env in sizeofType refType

genFunDef :: (MonadModuleBuilder m, MonadFix m) => Env -> M.Identifier -> M.Fun -> m Operand
genFunDef env funId (M.Fun params retType body) = do
  let paramsL =
        map
          ( \(M.FunParam name type') ->
              (genType env type', fromString $ M.idName name)
          )
          params
  let retTypeL = genType env retType
  function (mkName (M.idName funId)) paramsL retTypeL $ \argsL -> do
    _ <- block `named` "entry"
    argEnvEntries <-
      zip params argsL
        & mapM
          ( \(param, argL) -> do
              addr <- alloca (genType env $ M.paramType param) Nothing alignment
              store addr alignment argL
              pure (M.paramName param, addr)
          )
    let extendedEnv = env {oenv = oenv env <> Map.fromList argEnvEntries}
    bodyL <- genExp extendedEnv body
    ret bodyL

genExp :: (MonadModuleBuilder m, MonadIRBuilder m, MonadFix m) => Env -> M.Exp -> m Operand
genExp env (M.WithType expr _) = case expr of
  M.Let binding value body -> do
    valueL <- genExp env value
    addr <- alloca (genType env (M.type' value)) Nothing alignment
    store addr alignment valueL
    let extendedEnv = env {oenv = Map.insert binding addr $ oenv env}
    genExp extendedEnv body
  M.VarRef ref -> do
    let addr = fromJust (Map.lookup ref $ oenv env)
    load addr alignment
  M.Eseq first rest -> genExp env first *> genExp env rest
  M.Assign binding value -> do
    let addr = fromJust (Map.lookup binding $ oenv env)
    valueL <- genExp env value
    store addr alignment valueL
    pure $ C.bit alignment
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
    let structType = fromJust $ Map.lookup structId $ tenv env
     in foldl'
          ( \partialStruct (ix, arg) -> do
              partialStructL <- partialStruct
              argL <- genExp env arg
              insertValue partialStructL argL [ix]
          )
          (pure $ ConstantOperand $ Constant.Undef structType)
          (zip [0 ..] args)
  M.StructDeref obj ix -> do
    objL <- genExp env obj
    extractValue objL [ix]
  M.UnionMake unionId tag value -> do
    let unionType = fromJust $ Map.lookup unionId $ tenv env
    let valueType = genType env $ M.type' value
    let tagL = C.int8 $ fromIntegral tag
    structAddr <- alloca unionType Nothing alignment
    rawValueAddr <- gep structAddr [C.int32 0, C.int32 1]
    valueAddr <- bitcast rawValueAddr (T.ptr valueType)
    valueL <- genExp env value
    store valueAddr alignment valueL
    tagAddr <- gep structAddr [C.int32 0, C.int32 0]
    store tagAddr alignment tagL
    load structAddr alignment
  M.Match unionId value cases -> mdo
    unionL <- genExp env value
    let numCases = NonEmpty.length cases
    phiArgs <-
      cases
        & NonEmpty.zip (0 :| [1 ..])
        & mapM
          ( \(caseIndex, M.MatchCase ix binding bindingType body) ->
              let caseBody = mdo
                    matchBlock <- block `named` "match.matched"
                    let unionValueType = fromJust $ Map.lookup unionId $ unionValueTypes env
                    rawAddr <- alloca unionValueType Nothing alignment
                    valueL <- extractValue unionL [1]
                    store rawAddr alignment valueL
                    let valueType = genType env bindingType
                    addr <- bitcast rawAddr (T.ptr valueType)
                    let extendedEnv = env {oenv = Map.insert binding addr $ oenv env}
                    bodyL <- genExp extendedEnv body
                    br retBlock
                    retBlock <- block `named` "match.ret"
                    br exitBlock
                    pure (bodyL, retBlock, matchBlock)
               in ( if caseIndex == numCases - 1
                      then mdo
                        br matchBlock
                        (bodyL, retBlock, matchBlock) <- caseBody
                        pure (bodyL, retBlock)
                      else mdo
                        tagL <- extractValue unionL [0]
                        condL <- icmp IntegerPredicate.EQ (C.int8 $ fromIntegral ix) tagL
                        condBr condL matchBlock notMatchBlock
                        (bodyL, retBlock, matchBlock) <- caseBody
                        notMatchBlock <- block `named` "match.not-matched"
                        pure (bodyL, retBlock)
                  )
          )

    exitBlock <- block `named` "match.exit"
    phi $ NonEmpty.toList phiArgs
  M.Alloc value -> do
    let boxedType = M.type' value
    let boxedTypeL = genType env boxedType
    rawAddr <- call (mallocRef env) [(C.int64 $ sizeof env boxedType, [])]
    addr <- bitcast rawAddr $ ptr boxedTypeL
    valueL <- genExp env value
    store addr alignment valueL
    pure addr
  M.Dealloc pointer -> do
    pointerL <- genExp env pointer
    _ <- call (freeRef env) [(pointerL, [])]
    genExp env (M.WithType M.UnitLiteral M.Unit)
  M.SetPointer pointer value -> do
    pointerL <- genExp env pointer
    valueL <- genExp env value
    store pointerL alignment valueL
    pure $ C.bit 0
  M.GetPointer pointer -> do
    pointerL <- genExp env pointer
    load pointerL alignment
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

        typeEnv <- sequence $ Map.mapWithKey (genTypeDef env) types
        functionEnv <- sequence $ Map.mapWithKey (genFunDef env) funs
        let unionValues = mapMaybe (typeToUnionValueType env) types
        let env = Env typeEnv functionEnv malloc free types unionValues

        function "main" [] T.i32 $ \_ -> do
          _ <- block `named` "entry"

          bodyRes <- genExp env body
          _ <- call printNum [(bodyRes, [])]
          ret $ C.int32 0
   in modl
