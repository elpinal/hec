module Refine.Asm where

import Data.Int

data Inst =
    IAdd Register Operand Operand
  | ISub Register Operand Operand
  | Branch Operand
  | Load Register Operand
  | Store Location Register
  | Ret Operand
  deriving (Eq, Show)

data Operand =
    Const Constant
  | Loc Location
  deriving (Eq, Show)

data Constant =
    CInt8 Int8
  | CInt16 Int16
  | CInt32 Int32
  | CInt64 Int64
  deriving (Eq, Show)

data Location =
    Reg Register
  | Mem Memory
  deriving (Eq, Show)

newtype Register = Register Int
  deriving (Eq, Show)

newtype Memory = Memory Int
  deriving (Eq, Show)
