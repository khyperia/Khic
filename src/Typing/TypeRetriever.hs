module Typing.TypeRetriever(getType) where

import Ast

getType :: Expression -> Type
getType (BinaryOp _ l _) = getType l
getType (Negation x) = getType x
getType (MethodCall expr _) = case getType expr of (FunctionType f _) -> f; x -> x
getType (Assignment _ expr) = getType expr
getType (Identifier ty _) = ty
getType (ConstantInteger ty _) = ty
getType (Cast c _) = c
