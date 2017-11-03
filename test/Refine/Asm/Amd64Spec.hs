module Refine.Asm.Amd64Spec where

import Test.Hspec

import qualified Data.ByteString.Lazy as B

import Refine.Asm
import Refine.Asm.Amd64

spec :: Spec
spec = do
  describe "encode" $
    it "encodes an assembly instruction to amd64 machine code" $ do
      let load9toRAX = B.pack [0x48, 0xb8, 0x09, 0, 0, 0, 0, 0, 0, 0]

      encode (Load (Register 0)              . Const $ CInt8 9) `shouldBe` load9toRAX
      encode (Load (Register $ fromEnum RAX) . Const $ CInt8 9) `shouldBe` load9toRAX

      encode (Load (Register 0) . Const $ CInt16 9) `shouldBe` load9toRAX
      encode (Load (Register 0) . Const $ CInt32 9) `shouldBe` load9toRAX
      encode (Load (Register 0) . Const $ CInt64 9) `shouldBe` load9toRAX

      encode (Load (Register 0) . Const $ CInt8 (-1)) `shouldBe` B.pack [0x48, 0xb8, 255, 0, 0, 0, 0, 0, 0, 0]
      encode (Load (Register 0) . Const $ CInt8 127) `shouldBe` B.pack [0x48, 0xb8, 127, 0, 0, 0, 0, 0, 0, 0]

      encode (Load (Register 0) . Const $ CInt16 $ 2^15 - 1) `shouldBe` B.pack [0x48, 0xb8, 0xff, 0x7f, 0, 0, 0, 0, 0, 0]
      encode (Load (Register 0) . Const $ CInt32 $ 2^31 - 1) `shouldBe` B.pack [0x48, 0xb8, 0xff, 0xff, 0xff, 0x7f, 0, 0, 0, 0]
      encode (Load (Register 0) . Const $ CInt64 $ 2^63 - 1) `shouldBe` B.pack [0x48, 0xb8, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]

      encode (Load (Register 0) . Loc . Reg $ Register 1)  `shouldBe` B.pack [0x48, 0x8b, 0xc1]
      encode (Load (Register 1) . Loc . Mem $ Memory IP 4) `shouldBe` B.pack [0x48, 0x8b, 0x0d, 0x04, 0, 0, 0]

      encode (IAdd (Register 1) (Loc . Reg $ Register 1) . Const $ CInt8 9) `shouldBe` B.pack [0x48, 0x81, 0xc1, 0x09, 0, 0, 0]
      encode (ISub (Register 1) (Loc . Reg $ Register 1) . Const $ CInt8 9) `shouldBe` B.pack [0x48, 0x81, 0xe9, 0x09, 0, 0, 0]

      encode (IAdd (Register 1) (Loc . Reg $ Register 1) . Loc $ Reg $ Register 2) `shouldBe` B.pack [0x48, 0x01, 0xd1]
      encode (ISub (Register 1) (Loc . Reg $ Register 1) . Loc $ Reg $ Register 2) `shouldBe` B.pack [0x48, 0x29, 0xd1]

      encode (IAdd (Register 1) (Loc . Reg $ Register 1) . Loc $ Mem $ Memory IP 8) `shouldBe` B.pack [0x48, 0x03, 0x05, 0x08, 0, 0, 0]
      encode (ISub (Register 1) (Loc . Reg $ Register 1) . Loc $ Mem $ Memory IP 8) `shouldBe` B.pack [0x48, 0x2b, 0x05, 0x08, 0, 0, 0]

  describe "encodeMachOSection64" $
    it "encodes section_64 of Mach-O" $ do
      let text = 0xc3 -- The RET instruction.
          len  = 0x01 -- Length of text.
          off :: Num a => a
          off  = 0x77 -- Offset to text.
      encodeMachOSection64 (textSection (B.singleton text) off)
        `shouldBe`
        B.pack
          [ 0x5f, 0x5f, 0x74, 0x65
          , 0x78, 0x74, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , 0x5f, 0x5f, 0x54, 0x45
          , 0x58, 0x54, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , len , 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , off , 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x04, 0x00, 0x80
          , 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00
          ]
