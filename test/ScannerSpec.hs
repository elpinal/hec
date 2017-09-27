module ScannerSpec where

import Test.Hspec

import Data.Bifunctor

import Text.ParserCombinators.Parsec
import Text.Parsec.Error

import Scanner

spec :: Spec
spec = do
  describe "scanString" $ do
    it "scans a string literal" $ do
      parse scanString "filename" "\"\"" `shouldBe` Right ""
      parse scanString "filename" "\"aaa\"" `shouldBe` Right "aaa"
      parse scanString "filename" "\"abc @*% D1\"" `shouldBe` Right "abc @*% D1"

    it "returns errors when the input does not start / end with a quote" $ do
      let isOk = null . errorMessages
      first isOk (parse scanString "filename" " \"\"") `shouldBe` Left False
      first isOk (parse scanString "filename" "\"") `shouldBe` Left False

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
