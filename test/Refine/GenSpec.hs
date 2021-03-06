module Refine.GenSpec where

import Test.Hspec

import qualified Refine.Asm as Asm
import Refine.Gen
import Refine.Inter

spec :: Spec
spec = do
  describe "gen" $ do
    it "generates Elacht assembly code from 3 address code" $ do
      runMachine (gen (Const $ CInt 3) []) 1 `shouldBe` (Asm.Const . Asm.CInt64 $ 3, [])

      {-
      runMachine (gen (Name "x") [BinAssign (Name "x") (Bin "-") (Name "y") (Name "z")]) 2
        `shouldBe`
        ( Asm.Reg . Asm.Register $ 0,
          [Asm.ISub (Asm.Register 0) (Asm.Reg $ Asm.Register 0) (Asm.Reg $ Asm.Register 1)]
        )
      -}
