module Typing.Typechecker (typecheck) where

import Ast
import qualified Data.Map.Strict as Map

type Context = Map.Map String Type

rettypeKeyword :: String
rettypeKeyword = "%RETTYPE%"

fuseType :: Type -> Type -> Type
fuseType Var _ = error "Var type still present after typechecking"
fuseType _ Var = error "Var type still present after typechecking"
fuseType x y
        | x == y = x
        | otherwise = error ("Types " ++ show x ++ " and " ++ show y ++ " are incompatible")

isNum :: Type -> Type
isNum x@(IntType _) = x
isNum x@(FloatType _) = x
isNum x@Var = x
isNum x = error ("Type " ++ show x ++ " expected to be a number")

typecheckExpr :: Context -> Expression -> (Expression, Type, Context)
typecheckExpr ctx0 (BinaryOp op l r) =
        let (lcheck, ltype, ctx1) = typecheckExpr ctx0 l;
            (rcheck, rtype, ctx2) = typecheckExpr ctx1 r in
        case op of
                Equality ->    (BinaryOp op lcheck rcheck, fuseType ltype rtype `seq` IntType 1, ctx2)
                NotEquality -> (BinaryOp op lcheck rcheck, fuseType ltype rtype `seq` IntType 1, ctx2)
                _ -> (BinaryOp op lcheck rcheck, isNum $ fuseType ltype rtype, ctx2)
typecheckExpr ctx0 (Negation x) =
        let (check, ty, ctx1) = typecheckExpr ctx0 x in
        (Negation check, isNum ty, ctx1)
typecheckExpr ctx0 (MethodCall expr args) =
        let argstype = map (typecheckExpr ctx0) args;
            (exprcheck, exprtype, ctx1) = typecheckExpr ctx0 expr; in
        case exprtype of
        (FunctionType returnType fnArgsType) ->
                let argsZip = zipWith fuseType fnArgsType (map second argstype) in
                (MethodCall exprcheck (map first argstype), argsZip `seq` returnType, ctx1)
        _ -> error "Left hand side of an invocation was not a function"
  where first (x, _, _) = x
        second (_, x, _) = x
typecheckExpr ctx0 (Assignment (Identifier idType var) expr) =
        let (exprcheck, exprtype, ctx1) = typecheckExpr ctx0 expr;
            fusedVarExpr = if idType == Var then exprtype else fuseType exprtype idType;
            fusedMap = Map.insertWith fuseType var fusedVarExpr ctx1;
            fusedType = fusedMap Map.! var in
        (Assignment (Identifier fusedType var) exprcheck, fusedType, fusedMap)
typecheckExpr _ (Assignment _ _) = error "Pattern matching is not supported (only a single identifier is allowed to the left of an expression)"
typecheckExpr ctx0 (Identifier _ str) =
        let ty = ctx0 Map.! str in (Identifier ty str, ty, ctx0)
typecheckExpr ctx0 x@(ConstantInteger ty _) = (x, IntType ty, ctx0)
typecheckExpr ctx0 x@(ConstantFloat ty _) = (x, FloatType ty, ctx0)
typecheckExpr ctx0 (Cast ty expr) =
        let (ccheck, _, ctx1) = typecheckExpr ctx0 expr in (Cast ty ccheck, ty, ctx1)

typecheckStmt :: Context -> Statement -> (Statement, Context)
typecheckStmt ctx (ExprStatement expr) = case expr of
        (MethodCall _ _) -> ck
        (Assignment _ _) -> ck
        _ -> error ("Statement " ++ show expr ++ " not allowed as statement")
  where ck = let (check, _, newCtx) = typecheckExpr ctx expr in
             (ExprStatement check, newCtx)
typecheckStmt ctx0 (IfStatement condition ifTrue ifFalse) =
        let (conditionCheck, conditionType, ctx1) = typecheckExpr ctx0 condition;
            (ifTrueCheck, _) = typecheckBlock ctx1 ifTrue; -- _ to undefine variables inside ifTrue/ifFalse blocks
            (ifFalseCheck, _) = typecheckBlock ctx1 ifFalse in
        case conditionType of
        (IntType 1) -> (IfStatement conditionCheck ifTrueCheck ifFalseCheck, ctx1)
        _ -> error "Condition of if statement must be boolean"
typecheckStmt ctx0 (WhileStatement condition body) =
        let (conditionCheck, conditionType, ctx1) = typecheckExpr ctx0 condition;
            (bodyCheck, _) = typecheckBlock ctx1 body in
        case conditionType of
        (IntType 1) -> (WhileStatement conditionCheck bodyCheck, ctx1)
        _ -> error "Condition of while statement must be boolean"
typecheckStmt ctx0 (Return (Just value)) =
        let (valueCheck, valueType, ctx1) = typecheckExpr ctx0 value;
            typechecked = fuseType valueType (ctx1 Map.! rettypeKeyword) in
        typechecked `seq` (Return (Just valueCheck), ctx1)
typecheckStmt ctx0 (Return Nothing) =
        let typechecked = fuseType VoidType (ctx0 Map.! rettypeKeyword) in
        typechecked `seq` (Return Nothing, ctx0)

typecheckBlock :: Context -> Block -> (Block, Context)
typecheckBlock ctx [] = ([], ctx)
typecheckBlock ctx0 (stmt:stmts) = let (newstmt, ctx1) = typecheckStmt ctx0 stmt;
                                       (newblock, ctx2) = typecheckBlock ctx1 stmts in
                                       (newstmt : newblock, ctx2)

typecheckTld :: Context -> TopLevelDeclaration -> TopLevelDeclaration
typecheckTld ctx0 (Function retType fnName args (Just body)) =
        let ctx1 = Map.insert rettypeKeyword retType ctx0;
            ctx2 = foldl (\ctxTemp (ty, arg) -> Map.insert arg ty ctxTemp) ctx1 args;
            (bodyCheck, _) = typecheckBlock ctx2 body in
        Function retType fnName args (Just bodyCheck)
typecheckTld _ x@(Function _ _ _ Nothing) = x

buildInitialContext :: Program -> Context
buildInitialContext = foldl (\ctx x ->
        let (name, ty) = getNameTy x in Map.insertWith (\l r -> error ("Clashing names " ++ show l ++ " and " ++ show r)) name ty ctx) Map.empty
  where getNameTy (Function retType fnName args _) = (fnName, FunctionType retType (map fst args))

typecheck :: Program -> Program
typecheck program =
        let ctx = buildInitialContext program in
        map (typecheckTld ctx) program
