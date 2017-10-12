module Refine.Asm where

data Inst =
    IAdd Register Operand Operand
  | ISub Register Operand Operand
  | Branch Label
  | Load Register Operand
  | Store Register Operand
  | Call Label
  | Ret
  | Param Operand

newtype Label = Label Int

data Operand =
    Const Constant
  | Reg Register

data Constant =
    CInt Int
  | CBool Bool
  | CChar Char

newtype Register = Register Int
