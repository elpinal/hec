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
      parseExpr "succ 1" `shouldSatisfy` const False ||| (== App Succ (Num 1))
      parseExpr "toInt False" `shouldSatisfy` const False ||| (== App ToInt (Bool False))
      parseExpr "succ (toInt True)" `shouldSatisfy` const False ||| (== App Succ (App ToInt (Bool True)))

    it "returns an error when extra tokens appear" $ do
      parseExpr "1succ" `shouldSatisfy` isLeft
      parseExpr "succA" `shouldSatisfy` isLeft
