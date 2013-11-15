module Typing.PhiAnalyzer (getChangedVars) where

import Ast
import BasicBlockAst
import Data.List

analyzeExpr :: Expression -> [(String, Type)]
analyzeExpr (Assignment (Identifier ty s) expr) = (s, ty) : analyzeExpr expr
analyzeExpr (Assignment _ expr) = analyzeExpr expr
analyzeExpr (BinaryOp _ left right) = analyzeExpr left ++ analyzeExpr right
analyzeExpr (Negation expr) = analyzeExpr expr
analyzeExpr (MethodCall _ expr args) = analyzeExpr expr ++ concatMap analyzeExpr args
analyzeExpr (Cast _ expr) = analyzeExpr expr
analyzeExpr (Identifier _ _) = []
analyzeExpr (ConstantInteger _ _) = []
analyzeExpr (ConstantFloat _ _) = []

analyze :: [Expression] -> [(String, Type)]
analyze = foldr ((++) . analyzeExpr) []

getChangedVars :: BBAst -> [(String, Type)]
getChangedVars (Ret exprs (Just expr)) = nub $ analyze (expr:exprs)
getChangedVars (Ret exprs Nothing) = nub $ analyze exprs
getChangedVars (Drop exprs) = nub $ analyze exprs
getChangedVars (Branch exprs cond ifTrue ifFalse cont) = nub $ analyze (cond : exprs) ++ getChangedVars ifTrue ++ getChangedVars ifFalse ++ getChangedVars cont
getChangedVars (Loop exprs cond body cont) = nub $ analyze (cond : exprs) ++ getChangedVars body ++ getChangedVars cont
