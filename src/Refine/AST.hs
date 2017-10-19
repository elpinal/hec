module Refine.AST where

data Literal =
    LitInt Int
  | LitBool Bool
  | LitChar Char
  | LitString String
  deriving (Eq, Show)

data Expr =
    Lit Literal
  | BinOp String Expr Expr
  | App Expr Expr
  | Var String
  | Abs String Expr
    deriving (Eq, Show)
