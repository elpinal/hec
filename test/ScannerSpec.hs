module ScannerSpec where

import Test.Hspec

import Control.Exception
import Data.Bifunctor
import Data.Either

import Text.ParserCombinators.Parsec
import Text.Parsec.Error

import Scanner

spec :: Spec
spec = do
  describe "fromNum" $ do
    it "extracts a number from a token which have Num as its terminal symbol" $ do
      let toMaybe :: Either a b -> Maybe b
          toMaybe (Right x) = Just x
          toMaybe _ = Nothing
      fromNum (createToken "0" Num) `shouldSatisfy` (== Just 0) . toMaybe
      fromNum (createToken "18" Num) `shouldSatisfy` (== Just 18) . toMaybe
      fromNum (createToken "-18" Num) `shouldSatisfy` (== Just (-18)) . toMaybe

      fromNum (createToken "0x1a" Num) `shouldSatisfy` (== Just 0x1a) . toMaybe
      fromNum (createToken "0X1a" Num) `shouldSatisfy` (== Just 0x1a) . toMaybe

      fromNum (createToken "0o17" Num) `shouldSatisfy` (== Just 0o17) . toMaybe
      fromNum (createToken "0O17" Num) `shouldSatisfy` (== Just 0o17) . toMaybe

      fromNum (createToken "3.1" Num) `shouldSatisfy` (== Just 3.1) . toMaybe
      fromNum (createToken "3.1e+8" Num) `shouldSatisfy` (== Just 3.1e+8) . toMaybe
      fromNum (createToken "3.1e-8" Num) `shouldSatisfy` (== Just 3.1e-8) . toMaybe

    it "returns an error when the terminal symbol in a token is not Num" $ do
      fromNum (createToken "" Str) `shouldSatisfy` (== Left True) . first isNotNumError
      fromNum (createToken "" WhiteSpace) `shouldSatisfy` (== Left True) . first isNotNumError

    it "returns an error when the literal in a token is not a valid number" $ do
      fromNum (createToken "" Num) `shouldSatisfy` (== Left True) . first isReadError
      fromNum (createToken "a" Num) `shouldSatisfy` (== Left True) . first isReadError
      fromNum (createToken "1a" Num) `shouldSatisfy` (== Left True) . first isReadError

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
