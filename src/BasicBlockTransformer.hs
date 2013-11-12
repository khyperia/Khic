module BasicBlockTransformer (transformBB) where

import Ast
import BasicBlockAst

condense :: [Expression] -> Block -> BBAst -- previous is *reversed*
condense previous (ExprStatement current:rest) = condense (current : previous)  rest
condense previous (IfStatement condition ifTrue ifFalse:rest) = Branch (reverse previous) condition (condense [] ifTrue) (condense [] ifFalse) (condense [] rest)
condense previous (WhileStatement condition body:rest) = Loop (reverse previous) condition (condense [] body) (condense [] rest)
condense previous (Return expr:_) = Ret (reverse previous) expr
condense previous [] = Drop (reverse previous)

transformBB :: Block -> BBAst
transformBB = condense []
