module Ast where

data Type = Var
          | FunctionType Type [Type]
          | Structure [Type]
          | IntType Int
          | FloatType Int
          | VoidType
          | UnknownType String
        deriving(Eq, Show)

data BinaryOperation = Addition
                     | Subtraction
                     | Multiplication
                     | Division
                     | ShiftLeft
                     | ShiftRight
                     | Or
                     | And
        deriving(Eq, Show)

data Expression = BinaryOp BinaryOperation Expression Expression
                | Negation Expression
                | MethodCall Expression [Expression]
                | Assignment Expression Expression
                | Identifier Type String
                | ConstantInteger Type Integer
                | Cast Type Expression
        deriving(Eq, Show)

type Block = [Statement]

data Statement = IfStatement Expression Block Block
               | WhileStatement Expression Block
               | ExprStatement Expression
               | Return (Maybe Expression)
        deriving(Eq, Show)

data TopLevelDeclaration =
        Function Type String [(Type, String)] (Maybe Block)
        deriving(Eq, Show)

type Program = [TopLevelDeclaration]
