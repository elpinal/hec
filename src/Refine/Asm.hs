module Refine.Asm where

data Inst =
    IAdd Register Operand Operand
  | ISub Register Operand Operand
  | Branch Operand
  | Load Register Operand
  | Store Register Operand
  | Ret Operand
  deriving (Eq, Show)

data Operand =
    Const Constant
  | Reg Register
  | LValue String
  deriving (Eq, Show)

data Constant =
    CInt Int
  | CBool Bool
  | CChar Char
  deriving (Eq, Show)

newtype Register = Register Int
  deriving (Eq, Show)
