{-# LANGUAGE OverloadedStrings #-}
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

-- | Register for The instruction pointer.
data RIP = RIP

rex = 0x40

rexW = 8 -- W: 64 Bit Operand Size
rexR = 4
rexX = 2
rexB = 1

runRegister :: Register -> Word8
runRegister (Register n) = fromIntegral n

encode :: Inst -> B.ByteString

encode (Load r (Const c)) = packWithRexW [0xb8 + runRegister r] `B.append` encodeConstAs64 c
encode (Load r (Loc l))   = packWithRexW [0x8b] `B.append` modRM l (runRegister r)

encode (IAdd r  (Loc (Reg r')) (Const c))                    | r  == r' = packWithRexW [0x81, 0xc0 + runRegister r] `B.append` encodeConstAs32 c
encode (IAdd r1 (Loc (Reg r')) (Loc (Reg r2)))               | r1 == r' = packWithRexW [0x01] `B.append` modRM (Reg r1) (runRegister r2)
encode (IAdd r  (Loc (Reg r')) (Loc (Mem (Memory IP disp)))) | r  == r' = packWithRexW [0x03, runRegister r .|. disp32] `B.append` encodeConstAs32 (CInt32 $ fromIntegral disp)

encode (ISub r  (Loc (Reg r')) (Const c))                    | r  == r' = packWithRexW [0x81, 0xe8 + runRegister r] `B.append` encodeConstAs32 c
encode (ISub r1 (Loc (Reg r')) (Loc (Reg r2)))               | r1 == r' = packWithRexW [0x29] `B.append` modRM (Reg r1) (runRegister r2)
encode (ISub r  (Loc (Reg r')) (Loc (Mem (Memory IP disp)))) | r  == r' = packWithRexW [0x2b, runRegister r .|. disp32] `B.append` encodeConstAs32 (CInt32 $ fromIntegral disp)

-- | 32-bit displacement (ModR/M byte: 00***101).
disp32 :: Word8
disp32 = 5

packWithRexW :: [Word8] -> B.ByteString
packWithRexW xs = B.pack $ (rex .|. rexW) : xs

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

modRM :: Location -> Word8 -> B.ByteString
modRM (Reg r)                reg = B.singleton $ shift 0x03 6 .|. shift reg 3 .|. runRegister r
modRM (Mem (Memory IP disp)) reg = (shift reg 3 .|. disp32) `B.cons` encodeConstAs32 (CInt32 $ fromIntegral disp)


{- 64-bit Mach header -}

macgOHeaderSize :: Num a => a
macgOHeaderSize = 24 -- 24 bytes

word32ToBytes :: Word32 -> B.ByteString
word32ToBytes = B.pack . intToWords

machOHeaderObject :: B.ByteString
machOHeaderObject = B.concat
  [ machOMagicNumber64
  , word32ToBytes machOAmd64
  , word32ToBytes machOAmd64All
  , word32ToBytes machOObject
  ]

-- | The 64-bit Mach magic number.
machOMagicNumber64 :: B.ByteString
machOMagicNumber64 = B.pack [0xcf, 0xfa, 0xed, 0xfe]

-- | 64 bit ABI of cpu type.
machOAbi64 :: Word32
machOAbi64 = 0x01000000

-- | Amd64 cpu type.
machOAmd64 :: Word32
machOAmd64 = 7 .|. machOAbi64

-- | Amd64 cpu subtype.
machOAmd64All :: Word32
machOAmd64All = 3

-- | Filetype constant for relocatable object files.
machOObject :: Word32
machOObject = 1

-- | Filetype constant for demand paged executable files.
machOExecute :: Word32
machOExecute = 2

machOLCSegment64 :: Word32
machOLCSegment64 = 0x19

machOLCSymtab :: Word32
machOLCSymtab = 0x02

machOLCDysymtab :: Word32
machOLCDysymtab = 0x0b

machOLCUuid :: Word32
machOLCUuid = 0x1b

data MachOSection64 = MachOSection64
  { sectname  :: B.ByteString
  , segname   :: B.ByteString
  , addr      :: Word64
  , size      :: Word64
  , offset    :: Word32
  , align     :: Word32
  , reloff    :: Word32
  , nreloc    :: Word32
  , flags     :: Word32
  , reserved1 :: Word32
  , reserved2 :: Word32
  , reserved3 :: Word32
  }

sectionAttrPureInstructions :: Word32
sectionAttrPureInstructions = 0x80000000

sectionAttrSomeInstructions :: Word32
sectionAttrSomeInstructions = 0x400

textSection :: B.ByteString -> Word32 -> MachOSection64
textSection text off = MachOSection64
  { sectname  = "__text" `B.append` B.pack (replicate 10 0x00) -- 10 bytes = 16 bytes - length of "__text" (6 bytes).
  , segname   = "__TEXT" `B.append` B.pack (replicate 10 0x00)
  , addr      = 0
  , size      = fromIntegral $ B.length text
  , offset    = off
  , align     = 2^0
  , reloff    = 0
  , nreloc    = 0
  , flags     = sectionAttrPureInstructions .|. sectionAttrSomeInstructions
  , reserved1 = 0
  , reserved2 = 0
  , reserved3 = 0
  }

-- | Virtual memory protection.
type Prot = Word8

read :: Prot
read = 1

write :: Prot
write = 2

execute :: Prot
execute = 4
