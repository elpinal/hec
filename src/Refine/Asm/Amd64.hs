module Refine.Asm.Amd64 where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Int
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

encodeConstAs64 :: Constant -> B.ByteString
encodeConstAs64 (CInt8 n) = fromIntegral n `B.cons` B.pack (replicate 7 0x00)
encodeConstAs64 (CInt16 n) = (B.pack . map fromIntegral . (intToWords 16 :: Int16 -> [Int16])) n `B.append` B.pack (replicate 6 0x00)
encodeConstAs64 (CInt32 n) = (B.pack . map fromIntegral . intToWords 32) n `B.append` B.pack (replicate 4 0x00)
encodeConstAs64 (CInt64 n) = B.pack . map fromIntegral $ intToWords 64 n

intToWords :: (Bits a, Integral a) => Int -> a -> [a]
intToWords 16 n = [n .&. 0x00ff, shift n (-8)]
intToWords 32 n = [n .&. 0x000000ff, shift (n .&. 0x0000ff00) (-8), shift (n .&. 0x00ff0000) (-16), shift (n .&. 0xff000000) (-24)]
intToWords 64 n =
  [ n .&. 0x00000000000000ff
  , n .&. 0x000000000000ff00
  , n .&. 0x0000000000ff0000
  , n .&. 0x00000000ff000000
  , n .&. 0x000000ff00000000
  , n .&. 0x0000ff0000000000
  , n .&. 0x00ff000000000000
  , n .&. 0xff00000000000000
  ]
