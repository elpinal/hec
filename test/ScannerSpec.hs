module ScannerSpec where

import Test.Hspec

import Control.Exception
import Data.Bifunctor

import Text.ParserCombinators.Parsec
import Text.Parsec.Error

import Scanner

spec :: Spec
spec = do
  describe "fromNum" $ do
    it "extracts a number from a token which have Num as its terminal symbol" $ do
      fromNum (createToken "0" Num) `shouldBe` 0
      fromNum (createToken "18" Num) `shouldBe` 18
      fromNum (createToken "-18" Num) `shouldBe` -18

      fromNum (createToken "0x1a" Num) `shouldBe` 0x1a
      fromNum (createToken "0X1a" Num) `shouldBe` 0x1a

      fromNum (createToken "0o17" Num) `shouldBe` 0o17
      fromNum (createToken "0O17" Num) `shouldBe` 0o17

      fromNum (createToken "3.1" Num) `shouldBe` 3.1
      fromNum (createToken "3.1e+8" Num) `shouldBe` 3.1e+8
      fromNum (createToken "3.1e-8" Num) `shouldBe` 3.1e-8

    it "returns an error when the terminal symbol in a token is not Num" $ do
      evaluate (fromNum $ createToken "" Str) `shouldThrow` anyErrorCall
      evaluate (fromNum $ createToken "" WhiteSpace) `shouldThrow` anyErrorCall

    it "returns an error when the literal in a token is not a valid number" $ do
      evaluate (fromNum $ createToken "" Num) `shouldThrow` anyException
      evaluate (fromNum $ createToken "a" Num) `shouldThrow` anyException
      evaluate (fromNum $ createToken "1a" Num) `shouldThrow` anyException

  describe "scanString" $ do
    it "scans a string literal" $ do
      parse scanString "filename" "\"\"" `shouldBe` Right ""
      parse scanString "filename" "\"aaa\"" `shouldBe` Right "aaa"
      parse scanString "filename" "\"abc @*% D1\"" `shouldBe` Right "abc @*% D1"

    it "returns errors when the input does not start / end with a quote" $ do
      let isOk = null . errorMessages
      parse scanString "filename" " \"\"" `shouldSatisfy` (== Left False) . first isOk
      parse scanString "filename" "\"" `shouldSatisfy` (== Left False) . first isOk

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
