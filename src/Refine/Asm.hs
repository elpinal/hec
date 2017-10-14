module Refine.Asm where

data Inst =
    IAdd Register Operand Operand
  | ISub Register Operand Operand
  | Branch Label
  | Load Register Operand
  | Store Register Operand
  | Call Label
  | Ret Operand
  | Param Operand
  deriving (Eq, Show)

newtype Label = Label Int
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
