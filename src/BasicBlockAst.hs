module BasicBlockAst where

import Ast

data BBAst =
          Ret [Expression] (Maybe Expression)
        | Branch [Expression] Expression BBAst BBAst BBAst
        | Loop [Expression] Expression BBAst BBAst
        | Drop [Expression]
        deriving(Eq, Show)
