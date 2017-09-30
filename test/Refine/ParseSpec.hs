module Refine.ParseSpec where

import Test.Hspec

import Control.Arrow
import Data.Either

import Text.ParserCombinators.Parsec

import Refine.Parse

spec :: Spec
spec =
  describe "parseExpr" $ do
    it "parses an expression" $ do
      parseExpr "1" `shouldSatisfy` const False ||| (== Num 1)
      parseExpr "succ1" `shouldSatisfy` const False ||| (== Succ (Num 1))
      parseExpr "toIntFalse" `shouldSatisfy` const False ||| (== ToInt (Bool False))
      parseExpr "succtoIntTrue" `shouldSatisfy` const False ||| (== Succ (ToInt (Bool True)))

    it "returns an error when extra tokens appear" $ do
      parseExpr "1succ" `shouldSatisfy` isLeft
      parseExpr "succA" `shouldSatisfy` isLeft
