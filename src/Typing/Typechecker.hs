module Typing.Typechecker where

import Ast
import qualified Data.Map as Map

type Context = Map.Map String Type

fuseType :: Type -> Type -> Type
fuseType Var x = x
fuseType x Var = x
fuseType x y
        | x == y = x
        | otherwise = error ("Types " ++ show x ++ " and " ++ show y ++ " are incompatible")

isNum :: Type -> Type
isNum x@(IntType _) = x
isNum x@(FloatType _) = x
isNum x@Var = x
isNum x = error ("Type " ++ show x ++ " expected to be a number")

typecheckExpr :: Context -> Expression -> (Expression, Type)
typecheckExpr ctx (BinaryOp op l r) =
        let (lcheck, ltype) = typecheckExpr ctx l;
            (rcheck, rtype) = typecheckExpr ctx r in
        (BinaryOp op lcheck rcheck, isNum $ fuseType ltype rtype)
typecheckExpr ctx (Negation x) =
        let (check, ty) = typecheckExpr ctx x in
        (Negation check, isNum ty)
typecheckExpr ctx (MethodCall expr args) =
        let (exprcheck, exprtype) = typecheckExpr ctx expr;
            argstype = map (typecheckExpr ctx) args in
        case exprtype of
        (FunctionType returnType fnArgsType) ->
                let argsZip = zipWith fuseType fnArgsType (map snd argstype) in
                (MethodCall exprcheck (map fst argstype), FunctionType returnType argsZip)
        _ -> error "Left hand side of an invocation was not a function"
typecheckExpr ctx (Assignment var expr) =
        let (varcheck, vartype) = typecheckExpr ctx var;
            (exprcheck, exprtype) = typecheckExpr ctx expr in
        (Assignment varcheck exprcheck, fuseType vartype exprtype)
typecheckExpr ctx (Identifier _ str) =
        let ty = ctx Map.! str in (Identifier ty str, ty)
typecheckExpr _ x@(ConstantInteger ty _) = (x, ty)
typecheckExpr ctx (Cast ty expr) =
        let (ccheck, _) = typecheckExpr ctx expr in (ccheck, ty)
