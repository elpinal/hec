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

          shouldBeRight :: (Read a, Num a, Eq a, Show a) => String -> a -> Expectation
          shouldBeRight s n = fromNum (createToken s Num) `shouldSatisfy` (== Just n) . toMaybe

      "0" `shouldBeRight` 0
      "18" `shouldBeRight` 18
      "-18" `shouldBeRight` (-18)

      "0x1a" `shouldBeRight` 0x1a
      "0X1a" `shouldBeRight` 0x1a

      "0o17" `shouldBeRight` 0o17
      "0O17" `shouldBeRight` 0o17

      "3.1" `shouldBeRight` 3.1
      "3.1e+8" `shouldBeRight` 3.1e+8
      "3.1e-8" `shouldBeRight` 3.1e-8

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
