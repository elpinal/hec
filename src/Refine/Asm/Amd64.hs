module Refine.Asm.Amd64 where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word

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

rex = 0x40

rexW = 8 -- W: 64 Bit Operand Size
rexR = 4
rexX = 2
rexB = 1

runRegister :: Register -> Word8
runRegister (Register n) = fromIntegral n

encode :: Inst -> B.ByteString

encode (Load r (Const c)) = B.pack [rex .|. rexW, 0xb8 + runRegister r] `B.append` encodeConstAs64 c
encode (Load r (Loc l)) = B.pack [rex .|. rexW, 0x8b] `B.snoc` modRM l (runRegister r)

encode (IAdd r (Loc (Reg r')) (Const c)) | r == r' = B.pack [rex .|. rexW, 0x81, 0xc0 + runRegister r] `B.append` encodeConstAs32 c
encode (IAdd r1 (Loc (Reg r')) (Loc (Reg r2))) | r1 == r' = B.pack [rex .|. rexW, 0x01, shift 0x03 6 .|. shift (runRegister r2) 3 .|. runRegister r1]

encode (ISub r (Loc (Reg r')) (Const c)) | r == r' = B.pack [rex .|. rexW, 0x81, 0xe8 + runRegister r] `B.append` encodeConstAs32 c

encodeConstAs64 :: Constant -> B.ByteString
encodeConstAs64 (CInt8 n) = fromIntegral n `B.cons` B.pack (replicate 7 0x00)
encodeConstAs64 (CInt16 n) = (B.pack . intToWords) n `B.append` B.pack (replicate 6 0x00)
encodeConstAs64 (CInt32 n) = (B.pack . intToWords) n `B.append` B.pack (replicate 4 0x00)
encodeConstAs64 (CInt64 n) = B.pack $ intToWords n

encodeConstAs32 :: Constant -> B.ByteString
encodeConstAs32 (CInt64 _) = error "encodeConstAs32: unexpected CInt64"
encodeConstAs32 n = B.take 4 $ encodeConstAs64 n

intToWords :: (FiniteBits a, Integral a) => a -> [Word8]
intToWords n = map (fromIntegral . (.&. 0xff)) . take (finiteBitSize n `div` 8) $ iterate (shift' 8) n

shift' i n = shift n (-i)

modRM :: Location -> Word8 -> Word8
modRM (Reg r) reg = shift 0x03 6 .|. shift reg 3 .|. runRegister r
