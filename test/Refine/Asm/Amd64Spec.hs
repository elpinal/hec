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
