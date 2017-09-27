module ScannerSpec where

import Test.Hspec

import Scanner

spec :: Spec
spec =
  describe "Token1" $ do
    it "implements Functor" $
      (+ 12) <$> Token1 "lit" 7
      `shouldBe`
      Token1 "lit" 19
    it "follows Functor's laws" $ do
      let token = Token1 "lit" 2
      fmap id token `shouldBe` id token
      let f = (* 3)
          g = (+ 12)
      fmap (f . g) token `shouldBe` (fmap f . fmap g) token
