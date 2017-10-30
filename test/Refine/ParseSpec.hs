module Refine.ParseSpec where

import Test.Hspec

import Data.Either

import Refine.AST
import Refine.Parse
import Refine.Type

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
      parseExpr "[]"           `shouldSatisfy` rightIs (Lit LitEmptyList)
      parseExpr "[ ]"          `shouldSatisfy` rightIs (Lit LitEmptyList)
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

      parseExpr "1 : []"     `shouldSatisfy` rightIs (BinOp ":" (int 1) $ Lit LitEmptyList)
      parseExpr "1 : 2 : []" `shouldSatisfy` rightIs (BinOp ":" (BinOp ":" (int 1) $ int 2) $ Lit LitEmptyList)

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
    it "parses a declaration" $ do
      parseWhole parseDecl "x = 12" `shouldSatisfy` rightIs (Decl "x" $ int 12)

      parseWhole parseDecl "f = \\x -> x + 12" `shouldSatisfy` rightIs (Decl "f" . Abs "x" . BinOp "+" (Var "x") $ int 12)
      parseWhole parseDecl "f x = x + 12"      `shouldSatisfy` rightIs (Decl "f" . Abs "x" . BinOp "+" (Var "x") $ int 12)

  describe "parseType" $
    it "parses a type" $ do
      parseWhole parseType "Int"                 `shouldSatisfy` rightIs tInt
      parseWhole parseType "Bool -> Char"        `shouldSatisfy` rightIs (fn tBool tChar)
      parseWhole parseType "Int -> Bool -> Char" `shouldSatisfy` rightIs (fn tInt $ fn tBool tChar)
      parseWhole parseType "Int->Bool->Char"     `shouldSatisfy` rightIs (fn tInt $ fn tBool tChar)

      parseWhole parseType' "(Int)"                   `shouldSatisfy` rightIs tInt
      parseWhole parseType' "((Int))"                 `shouldSatisfy` rightIs tInt
      parseWhole parseType' "(Int -> Bool)"           `shouldSatisfy` rightIs (fn tInt tBool)
      parseWhole parseType' "Int -> (Bool)"           `shouldSatisfy` rightIs (fn tInt tBool)
      parseWhole parseType' "(Int -> (Bool))"         `shouldSatisfy` rightIs (fn tInt tBool)
      parseWhole parseType' "Int -> (Bool -> Char)"   `shouldSatisfy` rightIs (fn tInt $ fn tBool tChar)
      parseWhole parseType' "(Int -> (Bool -> Char))" `shouldSatisfy` rightIs (fn tInt $ fn tBool tChar)
      parseWhole parseType' "(Int -> Bool) -> Char"   `shouldSatisfy` rightIs (fn (fn tInt tBool) tChar)
      parseWhole parseType' "((Int -> Bool) -> Char)" `shouldSatisfy` rightIs (fn (fn tInt tBool) tChar)

  describe "parseTypeSig" $
    it "parses a type signature declaration" $ do
      parseWhole parseTypeSig "i :: Int"                 `shouldSatisfy` rightIs (TypeSig "i" tInt)
      parseWhole parseTypeSig "f :: Bool -> Char"        `shouldSatisfy` rightIs (TypeSig "f" $ fn tBool tChar)
      parseWhole parseTypeSig "f::Bool->Char"            `shouldSatisfy` rightIs (TypeSig "f" $ fn tBool tChar)
      parseWhole parseTypeSig "g :: Int -> Bool -> Char" `shouldSatisfy` rightIs (TypeSig "g" . fn tInt $ fn tBool tChar)
      parseWhole parseTypeSig "g::Int ->  Bool  -> Char" `shouldSatisfy` rightIs (TypeSig "g" . fn tInt $ fn tBool tChar)

  describe "parseTypeDecl" $
    it "parses a type synonym" $ do
      parseWhole parseTypeDecl "type I = Int"                `shouldSatisfy` rightIs (TypeDecl "I" tInt)
      parseWhole parseTypeDecl "type Fn = Bool -> Char"      `shouldSatisfy` rightIs (TypeDecl "Fn" $ fn tBool tChar)
      parseWhole parseTypeDecl "type B = Int -> Int -> Bool" `shouldSatisfy` rightIs (TypeDecl "B" . fn tInt $ fn tInt tBool)
      parseWhole parseTypeDecl "type B=Int -> Int -> Bool"   `shouldSatisfy` rightIs (TypeDecl "B" . fn tInt $ fn tInt tBool)

  describe "parseEmptyList" $
    it "parses a empty list" $ do
      parseWhole parseEmptyList "[]"  `shouldSatisfy` rightIs LitEmptyList
      parseWhole parseEmptyList "[ ]" `shouldSatisfy` rightIs LitEmptyList

  describe "parseList'" $
    it "parses a list" $ do
      parseWhole parseList' "[]"             `shouldSatisfy` rightIs []
      parseWhole parseList' "[1]"            `shouldSatisfy` rightIs [int 1]
      parseWhole parseList' "[False,True]"   `shouldSatisfy` rightIs [bool False, bool True]
      parseWhole parseList' "[False, True]"  `shouldSatisfy` rightIs [bool False, bool True]
      parseWhole parseList' "[False , True]" `shouldSatisfy` rightIs [bool False, bool True]
      parseWhole parseList' "[ 1 , 2 ]"      `shouldSatisfy` rightIs [int 1, int 2]

  describe "parseCase" $
    it "parses a case" $ do
      parseWhole parseCase "case 1 of 1 -> 1"     `shouldSatisfy` rightIs (Case (int 1) [(PLit (LitInt 1), int 1)])
      parseWhole parseCase "case 1 + 2 of 1 -> 1" `shouldSatisfy` rightIs (Case (BinOp "+" (int 1) $ int 2) [(PLit (LitInt 1), int 1)])
      parseWhole parseCase "case 1 of _ -> 1"     `shouldSatisfy` rightIs (Case (int 1) [(PWildcard, int 1)])
      parseWhole parseCase "case 1 of n -> n"     `shouldSatisfy` rightIs (Case (int 1) [(PVar "n", Var "n")])

  describe "keyword" $ do
    it "parses a keyword" $ do
      parse' (keyword "case") "case" `shouldSatisfy` rightIs "case"
      parse' (keyword "ABC")  "ABC"  `shouldSatisfy` rightIs "ABC"
      parse' (keyword "let'") "let'" `shouldSatisfy` rightIs "let'"
      parse' (keyword "  ")   "  "   `shouldSatisfy` rightIs "  "
      parse' (keyword "aaa1") "aaa1" `shouldSatisfy` rightIs "aaa1"
      parse' (keyword "a1b")  "a1b"  `shouldSatisfy` rightIs "a1b"
      parse' (keyword "1 ")   "1 "   `shouldSatisfy` rightIs "1 "

    it "parses a keyword even if followed by spaces or symbols" $ do
      parse' (keyword "case") "case is" `shouldSatisfy` rightIs "case"
      parse' (keyword "case") "case@@"  `shouldSatisfy` rightIs "case"
      parse' (keyword "case") "case()"  `shouldSatisfy` rightIs "case"
      parse' (keyword "ABC")  "ABC DE"  `shouldSatisfy` rightIs "ABC"
      parse' (keyword "ABC")  "ABC#@"   `shouldSatisfy` rightIs "ABC"
      parse' (keyword "let'") "let'&"   `shouldSatisfy` rightIs "let'"
      parse' (keyword "let'") "let' "   `shouldSatisfy` rightIs "let'"
      parse' (keyword "a1b")  "a1b "    `shouldSatisfy` rightIs "a1b"
      parse' (keyword "1 ")   "1 @"     `shouldSatisfy` rightIs "1 "
      parse' (keyword "1 ")   "1  "     `shouldSatisfy` rightIs "1 "

    it "fails if it is an identifier" $ do
      parse' (keyword "case") "cases" `shouldSatisfy` isLeft
      parse' (keyword "case") "case'" `shouldSatisfy` isLeft
      parse' (keyword "case") "caseA" `shouldSatisfy` isLeft
      parse' (keyword "case") "case1" `shouldSatisfy` isLeft
      parse' (keyword "case") "cas"   `shouldSatisfy` isLeft
      parse' (keyword "ABC")  "ABCD"  `shouldSatisfy` isLeft
      parse' (keyword "ABC")  "ABCd"  `shouldSatisfy` isLeft
      parse' (keyword "ABC")  "ABC'"  `shouldSatisfy` isLeft
      parse' (keyword "  ")   "  '"   `shouldSatisfy` isLeft
      parse' (keyword "  ")   "  a"   `shouldSatisfy` isLeft
      parse' (keyword "1 ")   "1 a"   `shouldSatisfy` isLeft
