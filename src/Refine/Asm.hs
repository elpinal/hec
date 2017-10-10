module Refine.Asm where

data Inst =
    IAdd Operand Operand Operand
  | ISub Operand Operand Operand
  | Branch Label
  | Load Operand Operand
  | Store Operand Operand

newtype Label = Label Int

data Operand =
    Const Constant
  | Reg Register

newtype Register = Register Int
