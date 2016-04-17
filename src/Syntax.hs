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
          | Dispatch Expr (Maybe Type) Name [Expr]
          | Function Name [Expr]
          | IfThenElse Expr Expr Expr
          | While Expr Expr
          | Block [Expr]
          | Let [(Formal, Maybe Expr)] Expr
          | Case Expr [(Formal, Expr)]
          | New Type
          | IsVoid Expr
          | BinaryOp String Expr Expr
          | UnaryOp String Expr
          | Braced Expr
          | ID Name
          | Int Integer
          | String String
          | Boolean Bool
          deriving (Eq, Ord, Show)




