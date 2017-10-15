module Refine.ParseSpec where

import Test.Hspec

import Data.Either

import Refine.Parse
import Refine.Type

int :: Int -> Expr
int = Lit . LitInt

bool :: Bool -> Expr
bool = Lit . LitBool

rightIs :: Eq a => a -> Either e a -> Bool
rightIs x (Right y) = x == y
rightIs _ (Left _) = False

spec :: Spec
spec = do
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
      parseExpr "f g True"     `shouldSatisfy` rightIs (App (App (Var "f") (Var "g")) (bool True))
      parseExpr "f (g True)"   `shouldSatisfy` rightIs (App (Var "f") (App (Var "g") (bool True)))
      parseExpr "f ( g True )" `shouldSatisfy` rightIs (App (Var "f") (App (Var "g") (bool True)))

    it "parses binary operations" $ do
      parseExpr "1#2"     `shouldSatisfy` rightIs (BinOp "#" (int 1) (int 2))
      parseExpr "1 # 2"   `shouldSatisfy` rightIs (BinOp "#" (int 1) (int 2))
      parseExpr "1 #$! 2" `shouldSatisfy` rightIs (BinOp "#$!" (int 1) (int 2))

      let want = BinOp "!" (BinOp "#" (int 1) $ int 2) $ int 3
      parseExpr "1 # 2 ! 3"       `shouldSatisfy` rightIs want
      parseExpr "(1 # 2 ! 3)"     `shouldSatisfy` rightIs want
      parseExpr "( 1 # 2 ! 3 )"   `shouldSatisfy` rightIs want
      parseExpr "((1 # 2 ! 3))"   `shouldSatisfy` rightIs want
      parseExpr "( (1 # 2 ! 3) )" `shouldSatisfy` rightIs want

      let want = let
                   x = App (Var "f") . App (Var "g") . bool $ True
                   y = App (Var "f") . int $ 2
                 in
                   BinOp "##" (BinOp "!" (BinOp "#" (int 1) y) x) $ int 3

      parseExpr "1 # f 2 ! f (g True) ## 3" `shouldSatisfy` rightIs want

      parseExpr "(1 # 2) ! 3" `shouldSatisfy` rightIs (BinOp "!" (BinOp "#" (int 1) (int 2)) (int 3))
      parseExpr "1 # (2 ! 3)" `shouldSatisfy` rightIs (BinOp "#" (int 1) (BinOp "!" (int 2) (int 3)))

    it "parses lambda abstractions" $ do
      parseExpr "\\x -> x"      `shouldSatisfy` rightIs (Abs "x" $ Var "x")
      parseExpr "(\\x->x)"      `shouldSatisfy` rightIs (Abs "x" $ Var "x")
      parseExpr "( \\ x -> x )" `shouldSatisfy` rightIs (Abs "x" $ Var "x")

      parseExpr "\\x -> x + x"        `shouldSatisfy` rightIs (Abs "x" . BinOp "+" (Var "x") $ Var "x")
      parseExpr "\\x -> \\y -> x + y" `shouldSatisfy` rightIs (Abs "x" . Abs "y" . BinOp "+" (Var "x") $ Var "y")

      parseExpr "map $ \\x -> x" `shouldSatisfy` rightIs (BinOp "$" (Var "map") . Abs "x" $ Var "x")

    it "returns an error when extra tokens appear" $ do
      parseExpr "1f"         `shouldSatisfy` isLeft
      parseExpr "TrueA"      `shouldSatisfy` isLeft
      parseExpr "f \\x -> x" `shouldSatisfy` isLeft

  describe "parseDecl" $
    it "parses a declaration" $
      parseWhole parseDecl "x = 12" `shouldSatisfy` rightIs (Decl "x" . Lit $ LitInt 12)

  describe "parseType" $
    it "parses a type" $ do
      parseWhole parseType "Int"                 `shouldSatisfy` rightIs TypeInt
      parseWhole parseType "Bool -> Char"        `shouldSatisfy` rightIs (TypeFun TypeBool TypeChar)
      parseWhole parseType "Int -> Bool -> Char" `shouldSatisfy` rightIs (TypeFun TypeInt $ TypeFun TypeBool TypeChar)
      parseWhole parseType "Int->Bool->Char"     `shouldSatisfy` rightIs (TypeFun TypeInt $ TypeFun TypeBool TypeChar)

      parseWhole parseType' "(Int)"                   `shouldSatisfy` rightIs TypeInt
      parseWhole parseType' "((Int))"                 `shouldSatisfy` rightIs TypeInt
      parseWhole parseType' "(Int -> Bool)"           `shouldSatisfy` rightIs (TypeFun TypeInt TypeBool)
      parseWhole parseType' "Int -> (Bool)"           `shouldSatisfy` rightIs (TypeFun TypeInt TypeBool)
      parseWhole parseType' "(Int -> (Bool))"         `shouldSatisfy` rightIs (TypeFun TypeInt TypeBool)
      parseWhole parseType' "Int -> (Bool -> Char)"   `shouldSatisfy` rightIs (TypeFun TypeInt $ TypeFun TypeBool TypeChar)
      parseWhole parseType' "(Int -> (Bool -> Char))" `shouldSatisfy` rightIs (TypeFun TypeInt $ TypeFun TypeBool TypeChar)
      parseWhole parseType' "(Int -> Bool) -> Char"   `shouldSatisfy` rightIs (TypeFun (TypeFun TypeInt TypeBool) TypeChar)
      parseWhole parseType' "((Int -> Bool) -> Char)" `shouldSatisfy` rightIs (TypeFun (TypeFun TypeInt TypeBool) TypeChar)

  describe "parseTypeSig" $
    it "parses a type signature declaration" $ do
      parseWhole parseTypeSig "i :: Int"                 `shouldSatisfy` rightIs (TypeSig "i" TypeInt)
      parseWhole parseTypeSig "f :: Bool -> Char"        `shouldSatisfy` rightIs (TypeSig "f" $ TypeFun TypeBool TypeChar)
      parseWhole parseTypeSig "g :: Int -> Bool -> Char" `shouldSatisfy` rightIs (TypeSig "g" . TypeFun TypeInt $ TypeFun TypeBool TypeChar)
      parseWhole parseTypeSig "g::Int ->  Bool  -> Char" `shouldSatisfy` rightIs (TypeSig "g" . TypeFun TypeInt $ TypeFun TypeBool TypeChar)

  describe "parseTypeDecl" $
    it "parses a type synonym" $ do
      parseWhole parseTypeDecl "type I = Int"                `shouldSatisfy` rightIs (TypeDecl "I" TypeInt)
      parseWhole parseTypeDecl "type Fn = Bool -> Char"      `shouldSatisfy` rightIs (TypeDecl "Fn" $ TypeFun TypeBool TypeChar)
      parseWhole parseTypeDecl "type B = Int -> Int -> Bool" `shouldSatisfy` rightIs (TypeDecl "B" . TypeFun TypeInt $ TypeFun TypeInt TypeBool)
      parseWhole parseTypeDecl "type B=Int -> Int -> Bool"   `shouldSatisfy` rightIs (TypeDecl "B" . TypeFun TypeInt $ TypeFun TypeInt TypeBool)
