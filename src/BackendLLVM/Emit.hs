module BackendLLVM.Emit (emit) where

import LLVM.General.AST as LAst
import qualified LLVM.General.AST.Type as LTy
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as Const
import qualified LLVM.General.AST.Float as Float
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.IntegerPredicate as IntPredicate
import qualified LLVM.General.AST.FloatingPointPredicate as FloatPredicate
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Ast
import BasicBlockAst
import BasicBlockTransformer
import Typing.TypeRetriever

data BBState = BBState { localContext :: Map.Map String Operand, currentLabel :: Int }
type EmitBBExpr a = WriterT [Named Instruction] (State BBState) a
type EmitBBAst a = WriterT [BasicBlock] (State BBState) a

genLabel :: (Monoid a) => WriterT a (State BBState) Name
genLabel = do
        modify (\st -> st{ currentLabel = currentLabel st + 1 })
        st <- get
        return . UnName . fromIntegral . currentLabel $ st

tellInstruction :: Instruction -> EmitBBExpr Operand
tellInstruction instruct = do
        lbl <- genLabel
        tell [lbl := instruct]
        return (LocalReference lbl)

emitExpr :: Expression -> EmitBBExpr Operand
emitExpr expr@(BinaryOp op leftExpr rightExpr) =
        let ty = getType expr in do
        left <- emitExpr leftExpr
        right <- emitExpr rightExpr
        tellInstruction (getOp ty op left right)
  where getOp (IntType _) Addition = \x y -> Add False False x y []
        getOp (IntType _) Subtraction = \x y -> Sub False False x y []
        getOp (IntType _) Multiplication = \x y -> Mul False False x y []
        getOp (IntType _) Division = \x y -> SDiv False x y []
        getOp (IntType _) ShiftLeft = \x y -> Shl False False x y []
        getOp (IntType _) ShiftRight = \x y -> AShr False x y []
        getOp (IntType _) Ast.And = \x y -> LAst.And x y []
        getOp (IntType _) Ast.Or = \x y -> LAst.Or x y []
        getOp (IntType _) Ast.Equality = \x y -> LAst.ICmp IntPredicate.EQ x y []
        getOp (IntType _) Ast.NotEquality = \x y -> LAst.ICmp IntPredicate.NE x y []
        getOp (FloatType _) Addition = \x y -> FAdd x y []
        getOp (FloatType _) Subtraction = \x y -> FSub x y []
        getOp (FloatType _) Multiplication = \x y -> FMul x y []
        getOp (FloatType _) Division = \x y -> FDiv x y []
        getOp (FloatType _) Ast.Equality = \x y -> LAst.FCmp FloatPredicate.OEQ x y []
        getOp (FloatType _) Ast.NotEquality = \x y -> LAst.FCmp FloatPredicate.ONE x y []
        getOp ty oper = error ("Cannot perform operation " ++ show oper ++ " on type " ++ show ty)
emitExpr (Negation expr) =
        case getType expr of
        (IntType s) -> emitExpr (BinaryOp Subtraction (ConstantInteger s 0) expr)
        (FloatType s) -> emitExpr (BinaryOp Subtraction (ConstantFloat s 0) expr)
        s -> error ("Bad type of negation: " ++ show s)
emitExpr (ConstantInteger numBits constValue) = return $ ConstantOperand (Const.Int (fromIntegral numBits) constValue)
emitExpr (ConstantFloat numBits constValue) = return $ ConstantOperand (Const.Float (getConst numBits constValue))
  where getConst 32 x = Float.Single (realToFrac x)
        getConst 64 x = Float.Double x
        getConst bitcount _ = error ("Cannot have " ++ show bitcount ++ " in a floating type. Allowed bits: 32, 64")
emitExpr (Identifier _ identifier) = liftM (lookupId . localContext) get
  where lookupId = fromMaybe (ConstantOperand . Const.GlobalReference $ Name identifier) . Map.lookup identifier
emitExpr (Assignment (Identifier _ left) rightExpr) = do
        right <- emitExpr rightExpr
        modify (\st -> st{ localContext = Map.insert left right (localContext st) })
        return right
emitExpr (Assignment _ _) = error "Typechecker did not catch patternmatch assignment"
emitExpr (Cast to valExpr) = 
        let from = getType valExpr in do
        val <- emitExpr valExpr
        cast from to val
  where cast (IntType fromSize) (IntType toSize) val
                | fromSize < toSize = tellInstruction (SExt val (IntegerType (fromIntegral toSize)) [])
                | fromSize > toSize = tellInstruction (Trunc val (IntegerType (fromIntegral toSize)) [])
                | otherwise = return val
        cast from tov _ = error ("Cannot cast from " ++ show from ++ " to " ++ show tov)
emitExpr (MethodCall funcExpr argsExpr) = do
        args <- mapM emitExpr argsExpr
        func <- emitExpr funcExpr
        tellInstruction (Call False CC.C [] (Right func) (map (\a -> (a, [])) args) [] []) -- TODO: Change CC.C to callconv

tellBlockInst :: [Named Instruction] -> Terminator -> EmitBBAst Name
tellBlockInst instructions terminator = do
        name <- genLabel
        tell [BasicBlock name instructions (Do terminator)]
        return name

tellBlockM :: [Expression] -> Expression -> (Operand -> EmitBBAst Terminator) -> EmitBBAst Name
tellBlockM exprsExpr termval termf = do
        (a, instructions) <- lift $ runWriterT (mapM_ emitExpr exprsExpr >> emitExpr termval) -- TODO: Possible bug here
        term <- termf a
        tellBlockInst instructions term

tellBlock :: [Expression] -> Expression -> (Operand -> Terminator) -> EmitBBAst Name
tellBlock exprsExpr termval termf = tellBlockM exprsExpr termval (return . termf)

tellBlockTerm :: [Expression] -> Terminator -> EmitBBAst Name
tellBlockTerm exprsExpr term = do
        (_, instructions) <- lift $ runWriterT (mapM_ emitExpr exprsExpr)
        tellBlockInst instructions term

emitBB :: Name -> BBAst -> EmitBBAst Name
emitBB jumpto (Drop exprsExpr) =
        tellBlockTerm exprsExpr (Br jumpto [])
emitBB _ (BasicBlockAst.Ret exprs (Just retExpr)) =
        tellBlock exprs retExpr (\retval -> LAst.Ret (Just retval) [])
emitBB _ (BasicBlockAst.Ret exprs Nothing) =
        tellBlockTerm exprs (LAst.Ret Nothing [])
emitBB jumpto (Branch exprs conditionExpr ifTrueExpr ifFalseExpr nextExpr) = do
        next <- emitBB jumpto nextExpr
        tellBlockM exprs conditionExpr (\cond -> do
                trueLabel <- emitBB next ifTrueExpr
                falseLabel <- emitBB next ifFalseExpr
                return $ LAst.CondBr cond trueLabel falseLabel [])
        
-- TODO: Fill in rest

convertType :: Ast.Type -> LAst.Type
convertType Var = error "Var type in Emit"
convertType (Ast.FunctionType returnType argsType) = LTy.FunctionType (convertType returnType) (map convertType argsType) False
convertType (Structure containedTypes) = LTy.StructureType False (map convertType containedTypes)
convertType (IntType numBits) = LTy.IntegerType (fromIntegral numBits)
convertType (FloatType numBits) = LTy.FloatingPointType (fromIntegral numBits) IEEE
convertType (Ast.VoidType) = LTy.VoidType
convertType (UnknownType s) = LTy.NamedTypeReference (Name s)

emitTld :: TopLevelDeclaration -> Definition
emitTld (Ast.Function retType fnName args (Just body)) =
        let bodyBlock = transformBB body;
            ((_, blocks), _) = flip runState BBState{ localContext = foldl (\m (_, str) -> Map.insert str (LocalReference (Name str)) m) Map.empty args, currentLabel = 1 } . runWriterT $
                (if retType == Ast.VoidType then
                        (do
                        val <- emitBB undefined (BasicBlockAst.Ret [] Nothing)
                        emitBB val bodyBlock)
                else emitBB  (error "Must have return value") bodyBlock) in
        GlobalDefinition functionDefaults {
                G.returnType = convertType retType,
                G.name = Name fnName,
                G.parameters = (map (\(t, s) -> Parameter (convertType t) (Name s) []) args, False),
                G.basicBlocks = reverse blocks
                }
emitTld (Ast.Function retType fnName args Nothing) =
        GlobalDefinition functionDefaults {
                G.returnType = convertType retType,
                G.name = Name fnName,
                G.parameters = (map (\(t, s) -> Parameter (convertType t) (Name s) []) args, False)
                }

emit :: Program -> Module
emit prgm =
        let globals = map emitTld prgm in
        Module "fzoo" Nothing Nothing globals



