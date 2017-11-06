module Refine.AST where

data Literal =
    LitInt Int
  | LitBool Bool
  | LitChar Char
  | LitString String
  | LitEmptyList
  | LitUnit
  deriving (Eq, Show)

data Expr =
    Lit Literal
  | BinOp String Expr Expr
  | App Expr Expr
  | Var String
  | Abs String Expr
  | Case Expr [(Pat, Expr)]
  | Tuple [Expr]
    deriving (Eq, Show)

data Pat =
    PVar String
  | PWildcard
  | PAs String Pat
  | PLit Literal
  {- FIXME: Don't depend on Assump.
     | PCon Assump [Pat]
  -}
  deriving (Eq, Show)

int :: Int -> Expr
int = Lit . LitInt

bool :: Bool -> Expr
bool = Lit . LitBool
