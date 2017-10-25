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
