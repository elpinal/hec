module Refine.Asm.Amd64 where

import Refine.Asm

-- | General perpose registers
data GPR =
    RAX
  | RBX
  | RCX
  | RDX
  | RDI
  | RSI
  | RBP
  | RSP
  -- R8 .. R15

instance Enum GPR where
  fromEnum RAX = 0x00
  fromEnum RBX = 0x03
  fromEnum RCX = 0x01
  fromEnum RDX = 0x02
  fromEnum RDI = 0x07
  fromEnum RSI = 0x06
  fromEnum RBP = 0x05
  fromEnum RSP = 0x04

  toEnum 0x00 = RAX
  toEnum 0x03 = RBX
  toEnum 0x01 = RCX
  toEnum 0x02 = RDX
  toEnum 0x07 = RDI
  toEnum 0x06 = RSI
  toEnum 0x05 = RBP
  toEnum 0x04 = RSP
