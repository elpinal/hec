module Refine.ParseSpec where

import Test.Hspec

import Control.Arrow
import Data.Either

import Refine.Parse

spec :: Spec
spec =
  describe "parseExpr" $ do
    it "parses an expression" $ do
      parseExpr "1" `shouldSatisfy` const False ||| (== Num 1)
      parseExpr "(1)" `shouldSatisfy` const False ||| (== Num 1)
      parseExpr "succ 1" `shouldSatisfy` const False ||| (== App Succ (Num 1))
      parseExpr "(succ 1)" `shouldSatisfy` const False ||| (== App Succ (Num 1))
      parseExpr "toInt False" `shouldSatisfy` const False ||| (== App ToInt (Bool False))
      parseExpr "succ (toInt True)" `shouldSatisfy` const False ||| (== App Succ (App ToInt (Bool True)))

    it "parses binary operations" $ do
      parseExpr "1#2" `shouldSatisfy` const False ||| (== BinOp "#" (Num 1) (Num 2))
      parseExpr "1 # 2" `shouldSatisfy` const False ||| (== BinOp "#" (Num 1) (Num 2))
      parseExpr "1 #$! 2" `shouldSatisfy` const False ||| (== BinOp "#$!" (Num 1) (Num 2))

      parseExpr "1 # 2 ! 3" `shouldSatisfy` const False ||| (== BinOp "!" (BinOp "#" (Num 1) (Num 2)) (Num 3))
      parseExpr "(1 # 2 ! 3)" `shouldSatisfy` const False ||| (== BinOp "!" (BinOp "#" (Num 1) (Num 2)) (Num 3))
      parseExpr "((1 # 2 ! 3))" `shouldSatisfy` const False ||| (== BinOp "!" (BinOp "#" (Num 1) (Num 2)) (Num 3))

      let want = BinOp "##" (BinOp "!" (BinOp "#" (Num 1) (App Succ (Num 2))) $ App Succ $ App ToInt $ Bool True) (Num 3)
      parseExpr "1 # succ 2 ! succ (toInt True) ## 3" `shouldSatisfy` const False ||| (== want)

      parseExpr "(1 # 2) ! 3" `shouldSatisfy` const False ||| (== BinOp "!" (BinOp "#" (Num 1) (Num 2)) (Num 3))
      parseExpr "1 # (2 ! 3)" `shouldSatisfy` const False ||| (== BinOp "#" (Num 1) (BinOp "!" (Num 2) (Num 3)))

    it "returns an error when extra tokens appear" $ do
      parseExpr "1succ" `shouldSatisfy` isLeft
      parseExpr "succA" `shouldSatisfy` isLeft
