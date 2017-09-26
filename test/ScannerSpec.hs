module ScannerSpec where

import Test.Hspec

import Scanner

spec :: Spec
spec = do
  describe "Token1" $
    it "implements Functor" $ do
      (+ 12) <$> Token1 "lit" 7
      `shouldBe`
      Token1 "lit" 19
