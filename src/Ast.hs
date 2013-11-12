module Ast where

data Type = Var
          | FunctionType Type [Type]
          | Structure [Type]
          | IntType Int
          | FloatType Int
          | VoidType
          | UnknownType String
        deriving(Eq, Show, Ord)

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
        deriving(Eq, Show, Ord)

data Expression = BinaryOp BinaryOperation Expression Expression
                | Negation Expression
                | MethodCall Expression [Expression]
                | Assignment Expression Expression
                | Identifier Type String
                | ConstantInteger Int Integer
                | ConstantFloat Int Double
                | Cast Type Expression
        deriving(Eq, Show, Ord)

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
