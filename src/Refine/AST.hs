module Refine.AST where

import Refine.Type.Syntactic

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
  | Field String
  | Record [(String, Expr)]
    deriving (Eq, Show)

data Pat =
    PVar String
  | PWildcard
  | PAs String Pat
  | PLit Literal
  | PCon String [Pat]
  deriving (Eq, Show)

data Decl =
    VarDecl String Expr
  | TypeAnn String Type
  | TypeDecl String Type
  | DataDecl String Type
  deriving (Eq, Show)

int :: Int -> Expr
int = Lit . LitInt

bool :: Bool -> Expr
bool = Lit . LitBool
