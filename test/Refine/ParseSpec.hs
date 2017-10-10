module Refine.ParseSpec where

import Test.Hspec

import Data.Either

import Refine.Parse

int :: Int -> Expr
int = Lit . LitInt

bool :: Bool -> Expr
bool = Lit . LitBool

rightIs :: Eq a => a -> Either e a -> Bool
rightIs x (Right y) = x == y
rightIs _ (Left _) = False

spec :: Spec
spec =
  describe "parseExpr" $ do
    it "parses an expression" $ do
      parseExpr "1"            `shouldSatisfy` rightIs (int 1)
      parseExpr "(1)"          `shouldSatisfy` rightIs (int 1)
      parseExpr "( 1 )"        `shouldSatisfy` rightIs (int 1)
      parseExpr "x"            `shouldSatisfy` rightIs (Var "x")
      parseExpr "'c'"          `shouldSatisfy` rightIs (Lit (LitChar 'c'))
      parseExpr "'\\''"        `shouldSatisfy` rightIs (Lit (LitChar '\''))
      parseExpr "\"\""         `shouldSatisfy` rightIs (Lit (LitString ""))
      parseExpr "\"aaa\""      `shouldSatisfy` rightIs (Lit (LitString "aaa"))
      parseExpr "\"\\\"\""     `shouldSatisfy` rightIs (Lit (LitString "\""))
      parseExpr "x 1"          `shouldSatisfy` rightIs (App (Var "x") (int 1))
      parseExpr "f 1"          `shouldSatisfy` rightIs (App (Var "f") (int 1))
      parseExpr "(f 1)"        `shouldSatisfy` rightIs (App (Var "f") (int 1))
      parseExpr "( f 1 )"      `shouldSatisfy` rightIs (App (Var "f") (int 1))
      parseExpr "g False"      `shouldSatisfy` rightIs (App (Var "g") (bool False))
      parseExpr "f (g True)"   `shouldSatisfy` rightIs (App (Var "f") (App (Var "g") (bool True)))
      parseExpr "f ( g True )" `shouldSatisfy` rightIs (App (Var "f") (App (Var "g") (bool True)))

    it "parses binary operations" $ do
      parseExpr "1#2"     `shouldSatisfy` rightIs (BinOp "#" (int 1) (int 2))
      parseExpr "1 # 2"   `shouldSatisfy` rightIs (BinOp "#" (int 1) (int 2))
      parseExpr "1 #$! 2" `shouldSatisfy` rightIs (BinOp "#$!" (int 1) (int 2))

      parseExpr "1 # 2 ! 3"       `shouldSatisfy` rightIs (BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))
      parseExpr "(1 # 2 ! 3)"     `shouldSatisfy` rightIs (BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))
      parseExpr "( 1 # 2 ! 3 )"   `shouldSatisfy` rightIs (BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))
      parseExpr "((1 # 2 ! 3))"   `shouldSatisfy` rightIs (BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))
      parseExpr "( (1 # 2 ! 3) )" `shouldSatisfy` rightIs (BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))

      let want = BinOp "##" (BinOp "!" (BinOp "#" (int 1) (App (Var "f") (int 2))) $ App (Var "f") $ App (Var "g") $ bool True) (int 3)
      parseExpr "1 # f 2 ! f (g True) ## 3" `shouldSatisfy` rightIs (want)

      parseExpr "(1 # 2) ! 3" `shouldSatisfy` rightIs (BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))
      parseExpr "1 # (2 ! 3)" `shouldSatisfy` rightIs (BinOp "#" (int 1) (BinOp "!" (int 2) (int 3)))

    it "returns an error when extra tokens appear" $ do
      parseExpr "1f"    `shouldSatisfy` isLeft
      parseExpr "TrueA" `shouldSatisfy` isLeft
