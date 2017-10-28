module Refine.AST where

data Literal =
    LitInt Int
  | LitBool Bool
  | LitChar Char
  | LitString String
  | LitEmptyList
  deriving (Eq, Show)

data Expr =
    Lit Literal
  | BinOp String Expr Expr
  | App Expr Expr
  | Var String
  | Abs String Expr
    deriving (Eq, Show)

data Pat =
    PVar String
  | PWildcard
  | PAs String Pat
  | PLit Literal
  -- FIXME: Don't depend on Assump.
  -- | PCon Assump [Pat]
