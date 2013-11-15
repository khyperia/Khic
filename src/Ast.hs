module Ast where

import qualified LLVM.General.AST.CallingConvention as CC

data Type = Var
          | FunctionType Type [Type]
          | Structure [Type]
          | IntType Int
          | FloatType Int
          | PointerType Type
          | VoidType
          | UnknownType String
        deriving(Eq, Show)

data BinaryOperation = Addition
                     | Subtraction
                     | Multiplication
                     | Division
                     | ShiftLeft
                     | ShiftRight
                     | Equality
                     | NotEquality
                     | Or
                     | And
        deriving(Eq, Show)

data Expression = BinaryOp BinaryOperation Expression Expression
                | Negation Expression
                | MethodCall CC.CallingConvention Expression [Expression]
                | Assignment Expression Expression
                | Identifier Type String
                | ConstantInteger Int Integer
                | ConstantFloat Int Double
                | Cast Type Expression
        deriving(Eq, Show)

type Block = [Statement]

data Statement = IfStatement Expression Block Block
               | WhileStatement Expression Block
               | ExprStatement Expression
               | Return (Maybe Expression)
        deriving(Eq, Show)

data TopLevelDeclaration =
        Function Type String CC.CallingConvention [(Type, String)] (Maybe Block)
        deriving(Eq, Show)

type Program = [TopLevelDeclaration]
