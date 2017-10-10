module Refine.Asm where

data Inst =
    IAdd Register Operand Operand
  | ISub Register Operand Operand
  | Branch Label
  | Load Register Operand
  | Store Register Operand

newtype Label = Label Int

data Operand =
    Const Constant
  | Reg Register

newtype Register = Register Int
