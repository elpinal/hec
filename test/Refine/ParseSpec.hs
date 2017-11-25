module Refine.ParseSpec where

import Test.Hspec

import Data.Either

import Refine.AST
import Refine.Kind
import Refine.Parse
import Refine.Type
import qualified Refine.Type.Syntactic as S

rightIs :: Eq a => a -> Either e a -> Bool
rightIs x (Right y) = x == y
rightIs _ (Left _) = False

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "parses an expression" $ do
      parseExpr "()"           `shouldSatisfy` rightIs (Lit LitUnit)
      parseExpr "( )"          `shouldSatisfy` rightIs (Lit LitUnit)
      parseExpr "(())"         `shouldSatisfy` rightIs (Lit LitUnit)
      parseExpr "((()))"       `shouldSatisfy` rightIs (Lit LitUnit)
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
      parseExpr "1f"           `shouldSatisfy` rightIs (App (int 1) $ Var "f")
      parseExpr "1 f"          `shouldSatisfy` rightIs (App (int 1) $ Var "f")
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

      parseExpr "1 `a` 2"   `shouldSatisfy` rightIs (BinOp "a" (int 1) (int 2))

    it "parses lambda abstractions" $ do
      parseExpr "\\x -> x"      `shouldSatisfy` rightIs (Abs "x" $ Var "x")
      parseExpr "(\\x->x)"      `shouldSatisfy` rightIs (Abs "x" $ Var "x")
      parseExpr "( \\ x -> x )" `shouldSatisfy` rightIs (Abs "x" $ Var "x")

      parseExpr "\\x -> x + x"        `shouldSatisfy` rightIs (Abs "x" . BinOp "+" (Var "x") $ Var "x")
      parseExpr "\\x -> \\y -> x + y" `shouldSatisfy` rightIs (Abs "x" . Abs "y" . BinOp "+" (Var "x") $ Var "y")

      parseExpr "map $ \\x -> x" `shouldSatisfy` rightIs (BinOp "$" (Var "map") . Abs "x" $ Var "x")

    it "returns an error when extra tokens appear" $ do
      parseExpr "TrueA"      `shouldSatisfy` isLeft
      parseExpr "f \\x -> x" `shouldSatisfy` isLeft

  describe "parseVarDecl" $ do
    it "parses a variable declaration" $ do
      parseWhole parseVarDecl "x = 12"            `shouldSatisfy` rightIs (VarDecl "x" $ int 12)
      parseWhole parseVarDecl "f = \\x -> x + 12" `shouldSatisfy` rightIs (VarDecl "f" . Abs "x" . BinOp "+" (Var "x") $ int 12)

    it "parses a function to which one argument is passed" $ do
      parseWhole parseVarDecl "f x = x + 12"   `shouldSatisfy` rightIs (VarDecl "f" . Abs "x" . BinOp "+" (Var "x") $ int 12)
      -- Currently two or more parameters are not implemented and under consideration.
      parseWhole parseVarDecl "f x y = x + y" `shouldSatisfy` isLeft

  describe "parseTypeIdent" $ do
    it "parses a type identifier" $ do
      parseWhole parseTypeIdent "A"          `shouldSatisfy` rightIs "A"
      parseWhole parseTypeIdent "AB"         `shouldSatisfy` rightIs "AB"
      parseWhole parseTypeIdent "X1"         `shouldSatisfy` rightIs "X1"
      parseWhole parseTypeIdent "Z0Z"        `shouldSatisfy` rightIs "Z0Z"
      parseWhole parseTypeIdent "A'"         `shouldSatisfy` rightIs "A'"
      parseWhole parseTypeIdent "A'b"        `shouldSatisfy` rightIs "A'b"
      parseWhole parseTypeIdent "A''"        `shouldSatisfy` rightIs "A''"
      parseWhole parseTypeIdent "A0'a9b''11" `shouldSatisfy` rightIs "A0'a9b''11"

    it "fails if it is not a type identifier" $ do
      parseWhole parseTypeIdent "a"   `shouldSatisfy` isLeft
      parseWhole parseTypeIdent "aBC" `shouldSatisfy` isLeft
      parseWhole parseTypeIdent "0"   `shouldSatisfy` isLeft
      parseWhole parseTypeIdent "'"   `shouldSatisfy` isLeft
      parseWhole parseTypeIdent "abc" `shouldSatisfy` isLeft
      parseWhole parseTypeIdent "@"   `shouldSatisfy` isLeft

  describe "parseType" $
    it "parses a type" $ do
      parseWhole parseType "Int"                 `shouldSatisfy` rightIs tInt
      parseWhole parseType "Bool -> Char"        `shouldSatisfy` rightIs (fn tBool tChar)
      parseWhole parseType "Int -> Bool -> Char" `shouldSatisfy` rightIs (fn tInt $ fn tBool tChar)
      parseWhole parseType "Int->Bool->Char"     `shouldSatisfy` rightIs (fn tInt $ fn tBool tChar)

  describe "parseType'" $
    it "parses a more complex type" $ do
      parseWhole parseType' "(Int)"                   `shouldSatisfy` rightIs tInt
      parseWhole parseType' "((Int))"                 `shouldSatisfy` rightIs tInt
      parseWhole parseType' "(Int -> Bool)"           `shouldSatisfy` rightIs (fn tInt tBool)
      parseWhole parseType' "Int -> (Bool)"           `shouldSatisfy` rightIs (fn tInt tBool)
      parseWhole parseType' "(Int -> (Bool))"         `shouldSatisfy` rightIs (fn tInt tBool)
      parseWhole parseType' "Int -> (Bool -> Char)"   `shouldSatisfy` rightIs (fn tInt $ fn tBool tChar)
      parseWhole parseType' "(Int -> (Bool -> Char))" `shouldSatisfy` rightIs (fn tInt $ fn tBool tChar)
      parseWhole parseType' "(Int -> Bool) -> Char"   `shouldSatisfy` rightIs (fn (fn tInt tBool) tChar)
      parseWhole parseType' "((Int -> Bool) -> Char)" `shouldSatisfy` rightIs (fn (fn tInt tBool) tChar)

      parseWhole parseType' "a"        `shouldSatisfy` rightIs (TypeVar $ TVar "a" Star)
      parseWhole parseType' "a -> Int" `shouldSatisfy` rightIs (TypeVar (TVar "a" Star) `fn` tInt)

      parseWhole parseType' "(Int, Bool)" `shouldSatisfy` rightIs (pair tInt tBool)
      parseWhole parseType' "(a, b)"      `shouldSatisfy` rightIs (pair (TypeVar $ TVar "a" Star) (TypeVar $ TVar "b" Star))

      parseWhole parseType' "()"  `shouldSatisfy` rightIs tUnit
      parseWhole parseType' "( )" `shouldSatisfy` rightIs tUnit

  describe "parseTypeAnn" $
    it "parses a type annotation declaration" $ do
      parseWhole parseTypeAnn "i :: Int"                 `shouldSatisfy` rightIs (TypeAnn "i" tInt)
      parseWhole parseTypeAnn "f :: Bool -> Char"        `shouldSatisfy` rightIs (TypeAnn "f" $ fn tBool tChar)
      parseWhole parseTypeAnn "f::Bool->Char"            `shouldSatisfy` rightIs (TypeAnn "f" $ fn tBool tChar)
      parseWhole parseTypeAnn "g :: Int -> Bool -> Char" `shouldSatisfy` rightIs (TypeAnn "g" . fn tInt $ fn tBool tChar)
      parseWhole parseTypeAnn "g::Int ->  Bool  -> Char" `shouldSatisfy` rightIs (TypeAnn "g" . fn tInt $ fn tBool tChar)

      parseWhole parseTypeAnn "x :: a"      `shouldSatisfy` rightIs (TypeAnn "x" $ TypeVar $ TVar "a" Star)
      parseWhole parseTypeAnn "x :: a -> a" `shouldSatisfy` rightIs (TypeAnn "x" $ (TypeVar $ TVar "a" Star) `fn` (TypeVar $ TVar "a" Star))

  describe "parseTypeDecl" $ do
    it "parses a type synonym" $ do
      parseWhole parseTypeDecl "type I = Int"                `shouldSatisfy` rightIs (TypeDecl "I" tInt)
      parseWhole parseTypeDecl "type Fn = Bool -> Char"      `shouldSatisfy` rightIs (TypeDecl "Fn" $ fn tBool tChar)
      parseWhole parseTypeDecl "type B = Int -> Int -> Bool" `shouldSatisfy` rightIs (TypeDecl "B" . fn tInt $ fn tInt tBool)
      parseWhole parseTypeDecl "type B=Int -> Int -> Bool"   `shouldSatisfy` rightIs (TypeDecl "B" . fn tInt $ fn tInt tBool)

    it "fails if given an illegal syntax" $
      parseWhole parseTypeDecl "typeI = Int" `shouldSatisfy` isLeft

  describe "parseDecl" $
    it "parses a declaration" $ do
      parseWhole parseDecl "x = 2"      `shouldSatisfy` rightIs (VarDecl "x" $ int 2)
      parseWhole parseDecl "f x = True" `shouldSatisfy` rightIs (VarDecl "f" $ Abs "x" $ bool True)

      parseWhole parseDecl "x :: Int" `shouldSatisfy` rightIs (TypeAnn "x" tInt)

      parseWhole parseDecl "type I = Int" `shouldSatisfy` rightIs (TypeDecl "I" tInt)

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

  describe "parseCase" $ do
    it "parses a case" $ do
      parseWhole parseCase "case 1 of 1 -> 1"     `shouldSatisfy` rightIs (Case (int 1) [(PLit (LitInt 1), int 1)])
      parseWhole parseCase "case 1 + 2 of 1 -> 1" `shouldSatisfy` rightIs (Case (BinOp "+" (int 1) $ int 2) [(PLit (LitInt 1), int 1)])
      parseWhole parseCase "case 1 of _ -> 1"     `shouldSatisfy` rightIs (Case (int 1) [(PWildcard, int 1)])
      parseWhole parseCase "case 1 of n -> n"     `shouldSatisfy` rightIs (Case (int 1) [(PVar "n", Var "n")])

      parseWhole parseCase "case 1of 1->1"        `shouldSatisfy` rightIs (Case (int 1) [(PLit (LitInt 1), int 1)])
      parseWhole parseCase "case(1 + 2)of 1 -> 1" `shouldSatisfy` rightIs (Case (BinOp "+" (int 1) $ int 2) [(PLit (LitInt 1), int 1)])

      parseWhole parseCase "case(s)of 1 -> 1" `shouldSatisfy` rightIs (Case (Var "s") [(PLit (LitInt 1), int 1)])
      parseWhole parseCase "case s of 1 -> 1" `shouldSatisfy` rightIs (Case (Var "s") [(PLit (LitInt 1), int 1)])

    it "fails if given an invalid syntax" $ do
      parseWhole parseCase "case1 of 1 -> 1" `shouldSatisfy` isLeft
      parseWhole parseCase "case 1 of1 -> 1" `shouldSatisfy` isLeft

      parseWhole parseCase "case 1 of 1 ->" `shouldSatisfy` isLeft
      parseWhole parseCase "case 1 of 1"    `shouldSatisfy` isLeft
      parseWhole parseCase "case 1 of"      `shouldSatisfy` isLeft
      parseWhole parseCase "case 1"         `shouldSatisfy` isLeft
      parseWhole parseCase "case"           `shouldSatisfy` isLeft
      parseWhole parseCase ""               `shouldSatisfy` isLeft
      parseWhole parseCase "1"              `shouldSatisfy` isLeft
      parseWhole parseCase "case of 1 -> 1" `shouldSatisfy` isLeft
      parseWhole parseCase "case 1 -> 1"    `shouldSatisfy` isLeft

      parseWhole parseCase "casesof1 -> 1"   `shouldSatisfy` isLeft
      parseWhole parseCase "case sof 1 -> 1" `shouldSatisfy` isLeft

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

  describe "varid" $ do
    it "parses a variable identifier" $ do
      parseWhole varid "a"              `shouldSatisfy` rightIs "a"
      parseWhole varid "abc"            `shouldSatisfy` rightIs "abc"
      parseWhole varid "abcDE"          `shouldSatisfy` rightIs "abcDE"
      parseWhole varid "abcDe"          `shouldSatisfy` rightIs "abcDe"
      parseWhole varid "abc123"         `shouldSatisfy` rightIs "abc123"
      parseWhole varid "abc1def"        `shouldSatisfy` rightIs "abc1def"
      parseWhole varid "abc'"           `shouldSatisfy` rightIs "abc'"
      parseWhole varid "abc'a"          `shouldSatisfy` rightIs "abc'a"
      parseWhole varid "abc'A2'12'3''4" `shouldSatisfy` rightIs "abc'A2'12'3''4"

      parseWhole varid "cas"    `shouldSatisfy` rightIs "cas"
      parseWhole varid "cases"  `shouldSatisfy` rightIs "cases"
      parseWhole varid "case'"  `shouldSatisfy` rightIs "case'"
      parseWhole varid "caseof" `shouldSatisfy` rightIs "caseof"
      parseWhole varid "types"  `shouldSatisfy` rightIs "types"
      parseWhole varid "cASE"   `shouldSatisfy` rightIs "cASE"

    it "fails if it is not an identifier" $ do
      parseWhole varid "1"      `shouldSatisfy` isLeft
      parseWhole varid ""       `shouldSatisfy` isLeft
      parseWhole varid " "      `shouldSatisfy` isLeft
      parseWhole varid "'"      `shouldSatisfy` isLeft
      parseWhole varid "A"      `shouldSatisfy` isLeft
      parseWhole varid "123+"   `shouldSatisfy` isLeft
      parseWhole varid "Abcd"   `shouldSatisfy` isLeft
      parseWhole varid "'11'aA" `shouldSatisfy` isLeft

    it "fails if it is keyword" $ do
      parseWhole varid "case" `shouldSatisfy` isLeft
      parseWhole varid "of"   `shouldSatisfy` isLeft
      parseWhole varid "type" `shouldSatisfy` isLeft

  describe "parseNewType" $
    it "parses a newtype declaration" $ do
      parseWhole parseNewType "newtype A = B Int" `shouldSatisfy` rightIs (NewTypeDecl "A" "B" tInt)
      parseWhole parseNewType "newtype A = A Int" `shouldSatisfy` rightIs (NewTypeDecl "A" "A" tInt)
      parseWhole parseNewType "newtype A=B Int"   `shouldSatisfy` rightIs (NewTypeDecl "A" "B" tInt)
      parseWhole parseNewType "newtype A = B a"   `shouldSatisfy` rightIs (NewTypeDecl "A" "B" (TypeVar $ TVar "a" Star))

  describe "parseTuple" $ do
    it "parses a tuple" $ do
      parseWhole parseTuple "(1,2)"     `shouldSatisfy` rightIs (Tuple [int 1, int 2])
      parseWhole parseTuple "(1, 2)"    `shouldSatisfy` rightIs (Tuple [int 1, int 2])
      parseWhole parseTuple "( 1 , 2 )" `shouldSatisfy` rightIs (Tuple [int 1, int 2])

      parseWhole parseTuple "(1, 2, 4)"           `shouldSatisfy` rightIs (Tuple [int 1, int 2, int 4])
      parseWhole parseTuple "(True, 3, 2, False)" `shouldSatisfy` rightIs (Tuple [bool True, int 3, int 2, bool False])

    it "can parse a nested tuple" $ do
      parseWhole parseTuple "((1, 3), True)"               `shouldSatisfy` rightIs (Tuple [Tuple [int 1, int 3], bool True])
      parseWhole parseTuple "((False, 3), (2, True))"      `shouldSatisfy` rightIs (Tuple [Tuple [bool False, int 3], Tuple [int 2, bool True]])
      parseWhole parseTuple "((1, 3), True, (1, 2, 3, 4))" `shouldSatisfy` rightIs (Tuple [Tuple [int 1, int 3], bool True, Tuple [int 1, int 2, int 3, int 4]])

    it "fails if given no tuple (n >= 2)" $ do
      parseWhole parseTuple "()"   `shouldSatisfy` isLeft
      parseWhole parseTuple "(1)"  `shouldSatisfy` isLeft
      parseWhole parseTuple "(1,)" `shouldSatisfy` isLeft

  describe "parseTupleType" $ do
    it "parses a tuple type" $ do
      parseWhole parseTupleType "(Int,Int)"      `shouldSatisfy` rightIs (tTupleN 2 `TypeApp` tInt `TypeApp` tInt)
      parseWhole parseTupleType "(Int, Bool)"    `shouldSatisfy` rightIs (pair tInt tBool)
      parseWhole parseTupleType "( Char , Int )" `shouldSatisfy` rightIs (pair tChar tInt)

      parseWhole parseTupleType "(Char, Int, Bool)" `shouldSatisfy` rightIs (tTupleN 3 `TypeApp` tChar `TypeApp` tInt `TypeApp` tBool)

    it "can parse a nested tuple type" $ do
      parseWhole parseTupleType "((Int, Char), Bool)"        `shouldSatisfy` rightIs (pair (pair tInt tChar) tBool)
      parseWhole parseTupleType "((Int, Int), (Bool, Bool))" `shouldSatisfy` rightIs (pair (pair tInt tInt) (pair tBool tBool))

  describe "parsePAs" $
    it "parses as-pattern" $ do
      parseWhole parsePAs "x @ 1"  `shouldSatisfy` rightIs (PAs "x" . PLit $ LitInt 1)
      parseWhole parsePAs "y' @ _" `shouldSatisfy` rightIs (PAs "y'" PWildcard)

      parseWhole parsePAs "x@1" `shouldSatisfy` rightIs (PAs "x" . PLit $ LitInt 1)

  describe "record" $
    it "parses a record" $ do
      parseWhole record "{}"                `shouldSatisfy` rightIs (Record [])
      parseWhole record "{a = 1}"           `shouldSatisfy` rightIs (Record [("a", int 1)])
      parseWhole record "{a = 0, b = True}" `shouldSatisfy` rightIs (Record [("a", int 0), ("b", bool True)])

      parseWhole record "{a = 0, a = True}"         `shouldSatisfy` rightIs (Record [("a", int 0), ("a", bool True)])
      parseWhole record "{a = 0, a = True, b' = 8}" `shouldSatisfy` rightIs (Record [("a", int 0), ("a", bool True), ("b'", int 8)])

      parseWhole record "{a=0,b=True}"         `shouldSatisfy` rightIs (Record [("a", int 0), ("b", bool True)])
      parseWhole record "{ a = 0 , b = True }" `shouldSatisfy` rightIs (Record [("a", int 0), ("b", bool True)])

  describe "recordType" $
    it "parses a record type" $ do
      parseWhole recordType "{}"                  `shouldSatisfy` rightIs (tRecordN [])
      parseWhole recordType "{a = Int}"           `shouldSatisfy` rightIs (tRecordN ["a"] `TypeApp` tInt)
      parseWhole recordType "{a = Int, b = Bool}" `shouldSatisfy` rightIs (tRecordN ["a", "b"] `TypeApp` tInt `TypeApp` tBool)

      parseWhole recordType "{a = Int, a = Bool}" `shouldSatisfy` rightIs (tRecordN ["a", "a"] `TypeApp` tInt `TypeApp` tBool)

      parseWhole recordType "{a = Int, b = Bool, c = Char}" `shouldSatisfy` rightIs (tRecordN ["a", "b", "c"] `TypeApp` tInt `TypeApp` tBool `TypeApp` tChar)

      parseWhole recordType "{a = Int, a = Int, a = Int}" `shouldSatisfy` rightIs (tRecordN ["a", "a", "a"] `TypeApp` tInt `TypeApp` tInt `TypeApp` tInt)

  describe "dataDecl" $ do
    it "parses a datatype declaration" $ do
      parseWhole dataDecl "data A = A Int" `shouldSatisfy` rightIs (DataDecl "A" [("A", [tInt])])
      parseWhole dataDecl "data A = B Int" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tInt])])

      parseWhole dataDecl "data A=B Int" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tInt])])

      parseWhole dataDecl "data A = B" `shouldSatisfy` rightIs (DataDecl "A" [("B", [])])

      parseWhole dataDecl "data A = B ()"   `shouldSatisfy` rightIs (DataDecl "A" [("B", [tUnit])])
      parseWhole dataDecl "data A = B (())" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tUnit])])

      parseWhole dataDecl "data A = B (Int, Bool)"   `shouldSatisfy` rightIs (DataDecl "A" [("B", [tTupleN 2 `TypeApp` tInt `TypeApp` tBool])])
      parseWhole dataDecl "data A = B ((Int), Bool)" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tTupleN 2 `TypeApp` tInt `TypeApp` tBool])])
      parseWhole dataDecl "data A = B (Int, ())"     `shouldSatisfy` rightIs (DataDecl "A" [("B", [tTupleN 2 `TypeApp` tInt `TypeApp` tUnit])])

      parseWhole dataDecl "data A = B ((), (), (Int, Bool))"
        `shouldSatisfy`
        rightIs (DataDecl "A" [("B", [tTupleN 3 `TypeApp` tUnit `TypeApp` tUnit `TypeApp` (tTupleN 2 `TypeApp` tInt `TypeApp` tBool)])])

      parseWhole dataDecl "data A = B {}" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN []])])
      parseWhole dataDecl "data A = B{}"  `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN []])])

      parseWhole dataDecl "data A = B {a = Int}"           `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["a"] `TypeApp` tInt])])
      parseWhole dataDecl "data A = B {a = Bool, b = Int}" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["a", "b"] `TypeApp` tBool `TypeApp` tInt])])
      parseWhole dataDecl "data A = B {b = Int, a = Bool}" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["b", "a"] `TypeApp` tInt `TypeApp` tBool])])
      parseWhole dataDecl "data A = B {a = Int, a = Bool}" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["a", "a"] `TypeApp` tInt `TypeApp` tBool])])
      parseWhole dataDecl "data A = B {a = Int, a = Int}"  `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["a", "a"] `TypeApp` tInt `TypeApp` tInt])])

      parseWhole dataDecl "data A = B Int Bool"      `shouldSatisfy` rightIs (DataDecl "A" [("B", [tInt, tBool])])
      parseWhole dataDecl "data A = B (Int -> Bool)" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tInt `fn` tBool])])

    it "parses a variant declaration" $ do
      parseWhole dataDecl "data A = B | C"     `shouldSatisfy` rightIs (DataDecl "A" [("B", []), ("C", [])])
      parseWhole dataDecl "data A=B|C"         `shouldSatisfy` rightIs (DataDecl "A" [("B", []), ("C", [])])
      parseWhole dataDecl "data A = B | C | D" `shouldSatisfy` rightIs (DataDecl "A" [("B", []), ("C", []), ("D", [])])

      -- FIXME
      parseWhole dataDecl "data A = B Int | C Bool" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tInt]), ("C", [tBool])])
      parseWhole dataDecl "data A = B Int | C"      `shouldSatisfy` rightIs (DataDecl "A" [("B", [tInt]), ("C", [])])
      parseWhole dataDecl "data A = B Int|C"        `shouldSatisfy` rightIs (DataDecl "A" [("B", [tInt]), ("C", [])])

    it "fails if given invalid syntax" $ do
      parseWhole dataDecl "dataA=B Int" `shouldSatisfy` isLeft

      parseWhole dataDecl "data A = B Int -> Bool" `shouldSatisfy` isLeft

      parseWhole dataDecl "data A =" `shouldSatisfy` isLeft
      parseWhole dataDecl "data A"   `shouldSatisfy` isLeft
      parseWhole dataDecl "data"     `shouldSatisfy` isLeft

  describe "infixed" $
    it "parses a infixed function" $ do
      parseWhole infixed "`a`"    `shouldSatisfy` rightIs "a"
      parseWhole infixed "`a'`"   `shouldSatisfy` rightIs "a'"
      parseWhole infixed "`a''`"  `shouldSatisfy` rightIs "a''"
      parseWhole infixed "`a0b`"  `shouldSatisfy` rightIs "a0b"
      parseWhole infixed "`qrst`" `shouldSatisfy` rightIs "qrst"

  describe "typeApp" $
    it "parses a type application" $ do
      let var = S.TypeVar
          con = S.TypeCon

      parseWhole typeApp "a" `shouldSatisfy` rightIs (var "a")
      parseWhole typeApp "A" `shouldSatisfy` rightIs (con "A")

      parseWhole typeApp "a b" `shouldSatisfy` rightIs (var "a" `S.TypeApp` var "b")
      parseWhole typeApp "A B" `shouldSatisfy` rightIs (con "A" `S.TypeApp` con "B")

      parseWhole typeApp "a b c" `shouldSatisfy` rightIs (var "a" `S.TypeApp` var "b" `S.TypeApp` var "c")
      parseWhole typeApp "A B C" `shouldSatisfy` rightIs (con "A" `S.TypeApp` con "B" `S.TypeApp` con "C")

      parseWhole typeApp "ab X ab bc a A" `shouldSatisfy` (rightIs . foldl1 S.TypeApp) [var "ab", con "X", var "ab", var "bc", var "a", con "A"]

  describe "typeFn" $ do
    let var = S.TypeVar
        con = S.TypeCon

    it "parses a type which may consist of functions" $ do
      parseWhole typeFn "A"      `shouldSatisfy` rightIs (con "A")
      parseWhole typeFn "A -> B" `shouldSatisfy` rightIs (con "A" `S.fn` con "B")

      parseWhole typeFn "a b"      `shouldSatisfy` rightIs (var "a" `S.TypeApp` var "b")
      parseWhole typeFn "a b -> C" `shouldSatisfy` rightIs (var "a" `S.TypeApp` var "b" `S.fn` con "C")

      parseWhole typeFn "A B C"            `shouldSatisfy` rightIs (con "A" `S.TypeApp` con "B" `S.TypeApp` con "C")
      parseWhole typeFn "A B C -> BC cC A" `shouldSatisfy` rightIs (foldl1 S.TypeApp [con "A", con "B", con "C"] `S.fn` foldl1 S.TypeApp [con "BC", var "cC", con "A"])

      parseWhole typeFn "a (b -> C)" `shouldSatisfy` rightIs (S.TypeApp (var "a") $ var "b" `S.fn` con "C")

      parseWhole typeFn "a b c"   `shouldSatisfy` rightIs (var "a" `S.TypeApp` var "b" `S.TypeApp` var "c")
      parseWhole typeFn "a (b c)" `shouldSatisfy` rightIs (S.TypeApp (var "a") $ var "b" `S.TypeApp` var "c")

      parseWhole typeFn "a (b -> C) -> aaaXXX" `shouldSatisfy` rightIs (S.TypeApp (var "a") (var "b" `S.fn` con "C") `S.fn` var "aaaXXX")

    it "parses some primitve type literals" $ do
      parseWhole typeFn "()"     `shouldSatisfy` rightIs S.tUnit
      parseWhole typeFn "(())"   `shouldSatisfy` rightIs S.tUnit
      parseWhole typeFn "((()))" `shouldSatisfy` rightIs S.tUnit

      parseWhole typeFn "() ()" `shouldSatisfy` rightIs (S.tUnit `S.TypeApp` S.tUnit)

      parseWhole typeFn "() -> ()"         `shouldSatisfy` rightIs (S.tUnit `S.fn` S.tUnit)
      parseWhole typeFn "() -> () -> ()"   `shouldSatisfy` rightIs (S.tUnit `S.fn` (S.tUnit `S.fn` S.tUnit))
      parseWhole typeFn "() -> (() -> ())" `shouldSatisfy` rightIs (S.tUnit `S.fn` (S.tUnit `S.fn` S.tUnit))
      parseWhole typeFn "(() -> ()) -> ()" `shouldSatisfy` rightIs (S.tUnit `S.fn` S.tUnit `S.fn` S.tUnit)

      parseWhole typeFn "(a, b)"    `shouldSatisfy` rightIs (S.tTupleN 2 `S.TypeApp` var "a" `S.TypeApp` var "b")
      parseWhole typeFn "(a, b, c)" `shouldSatisfy` rightIs (S.tTupleN 3 `S.TypeApp` var "a" `S.TypeApp` var "b" `S.TypeApp` var "c")

  describe "typeTerm" $
    it "parses a type term" $ do
      let var = S.TypeVar
          con = S.TypeCon

      parseWhole typeTerm "A"   `shouldSatisfy` rightIs (con "A")
      parseWhole typeTerm "(A)" `shouldSatisfy` rightIs (con "A")

      parseWhole typeTerm "(A -> B)"       `shouldSatisfy` rightIs (con "A" `S.fn` con "B")
      parseWhole typeTerm "((A) -> (B))"   `shouldSatisfy` rightIs (con "A" `S.fn` con "B")
      parseWhole typeTerm "(((A) -> (B)))" `shouldSatisfy` rightIs (con "A" `S.fn` con "B")

      parseWhole typeTerm "(a b)" `shouldSatisfy` rightIs (var "a" `S.TypeApp` var "b")

      parseWhole typeTerm "(a b -> C)"   `shouldSatisfy` rightIs (var "a" `S.TypeApp` var "b" `S.fn` con "C")
      parseWhole typeTerm "(a (b -> C))" `shouldSatisfy` rightIs (S.TypeApp (var "a") $ var "b" `S.fn` con "C")

      parseWhole typeTerm "(A B C)"   `shouldSatisfy` rightIs (foldl1 S.TypeApp [con "A", con "B", con "C"])
      parseWhole typeTerm "((A B) C)" `shouldSatisfy` rightIs (foldl1 S.TypeApp [con "A", con "B", con "C"])
      parseWhole typeTerm "(A (B C))" `shouldSatisfy` rightIs (foldr1 S.TypeApp [con "A", con "B", con "C"])

      parseWhole typeTerm "(A B C -> BC cC A)"   `shouldSatisfy` rightIs (foldl1 S.TypeApp [con "A", con "B", con "C"] `S.fn` foldl1 S.TypeApp [con "BC", var "cC", con "A"])
      parseWhole typeTerm "(A (B C -> BC) cC A)" `shouldSatisfy` (rightIs . foldl1 S.TypeApp) [con "A", con "B" `S.TypeApp` con "C" `S.fn` con "BC", var "cC", con "A"]

  describe "labeledType" $
    it "parses a labeled type" $ do
      parseWhole labeledType "A B"   `shouldSatisfy` rightIs ("A", [S.TypeCon "B"])
      parseWhole labeledType "A B C" `shouldSatisfy` rightIs ("A", [S.TypeCon "B", S.TypeCon "C"])
      parseWhole labeledType "A a"   `shouldSatisfy` rightIs ("A", [S.TypeVar "a"])
