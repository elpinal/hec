module Refine.ParseSpec where

import Test.Hspec

import Control.Arrow
import Data.Either

import Refine.Parse

int :: Int -> Expr
int = Lit . LitInt

bool :: Bool -> Expr
bool = Lit . LitBool

spec :: Spec
spec =
  describe "parseExpr" $ do
    it "parses an expression" $ do
      parseExpr "1" `shouldSatisfy` const False ||| (== int 1)
      parseExpr "(1)" `shouldSatisfy` const False ||| (== int 1)
      parseExpr "x" `shouldSatisfy` const False ||| (== Var "x")
      parseExpr "x 1" `shouldSatisfy` const False ||| (== App (Var "x") (int 1))
      parseExpr "f 1" `shouldSatisfy` const False ||| (== App (Var "f") (int 1))
      parseExpr "(f 1)" `shouldSatisfy` const False ||| (== App (Var "f") (int 1))
      parseExpr "g False" `shouldSatisfy` const False ||| (== App (Var "g") (bool False))
      parseExpr "f (g True)" `shouldSatisfy` const False ||| (== App (Var "f") (App (Var "g") (bool True)))

    it "parses binary operations" $ do
      parseExpr "1#2" `shouldSatisfy` const False ||| (== BinOp "#" (int 1) (int 2))
      parseExpr "1 # 2" `shouldSatisfy` const False ||| (== BinOp "#" (int 1) (int 2))
      parseExpr "1 #$! 2" `shouldSatisfy` const False ||| (== BinOp "#$!" (int 1) (int 2))

      parseExpr "1 # 2 ! 3" `shouldSatisfy` const False ||| (== BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))
      parseExpr "(1 # 2 ! 3)" `shouldSatisfy` const False ||| (== BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))
      parseExpr "((1 # 2 ! 3))" `shouldSatisfy` const False ||| (== BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))

      let want = BinOp "##" (BinOp "!" (BinOp "#" (int 1) (App (Var "f") (int 2))) $ App (Var "f") $ App (Var "g") $ bool True) (int 3)
      parseExpr "1 # f 2 ! f (g True) ## 3" `shouldSatisfy` const False ||| (== want)

      parseExpr "(1 # 2) ! 3" `shouldSatisfy` const False ||| (== BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))
      parseExpr "1 # (2 ! 3)" `shouldSatisfy` const False ||| (== BinOp "#" (int 1) (BinOp "!" (int 2) (int 3)))

    it "returns an error when extra tokens appear" $ do
      parseExpr "1f" `shouldSatisfy` isLeft
      parseExpr "TrueA" `shouldSatisfy` isLeft
