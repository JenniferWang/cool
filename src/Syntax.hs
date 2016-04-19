module Syntax where

type Name = String
type Type = String
type ParentType = Type

data Class = Class Type ParentType [Feature]
             deriving (Eq, Ord, Show)

data Feature = Method Formal Args Expr
             | Field Formal (Maybe Expr)
             deriving (Eq, Ord, Show)

data Formal = Formal Name Type
              deriving (Eq, Ord, Show)

type Args = [Formal]

data Expr = Assign Name Expr
          | Dispatch Expr Expr
          | StaticDispatch Type Expr Expr
          | Function Name [Expr]
          | IfThenElse Expr Expr Expr
          | While Expr Expr
          | Block [Expr]
          | Let [(Formal, Maybe Expr)] Expr
          | Case Expr [(Formal, Expr)]
          | New Type
          | IsVoid Expr
          | BinaryOp BOp Expr Expr
          | UnaryOp UOp Expr
          | ID Name
          | Int Integer
          | String String
          | Boolean Bool
          deriving (Eq, Ord, Show)

data BOp = Plus
         | Minus
         | Multiply
         | Divide
         | LessThan
         | LessEqual
         | Equal
         deriving (Eq, Ord, Show)

data UOp = Negate
         | Not
         deriving (Eq, Ord, Show)
