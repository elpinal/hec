module Refine.ParseSpec where

import Test.Hspec

import Control.Arrow

import Text.ParserCombinators.Parsec

import Refine.Parse

spec :: Spec
spec =
  describe "parseExpr" $
    it "parses an expression" $ do
      (parseExpr "1" :: Either ParseError (Expr Int)) `shouldSatisfy` const False ||| (== Num 1)
      (parseExpr "succ1" :: Either ParseError (Expr Int)) `shouldSatisfy` const False ||| (== Num 2)
      (parseExpr "toIntFalse" :: Either ParseError (Expr Int)) `shouldSatisfy` const False ||| (== Num 0)
      (parseExpr "succtoIntTrue" :: Either ParseError (Expr Int)) `shouldSatisfy` const False ||| (== Num 2)
