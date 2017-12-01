module Refine.ParseSpec where

import Test.Hspec
import Test.HUnit

import Data.Either

import Refine.AST
import Refine.Parse
import Refine.Type.Syntactic

rightIs :: Eq a => a -> Either e a -> Bool
rightIs x (Right y) = x == y
rightIs _ (Left _) = False

matchRight :: (HasCallStack, Eq a, Show a, Show e) => Either e a -> Maybe a -> Expectation
matchRight (Left e) Nothing = return ()
matchRight (Left e) (Just a) = do
  assertFailure $ "expected: " ++ show a ++ "\n" ++ show e
matchRight (Right x) Nothing = Just x `shouldBe` Nothing
matchRight (Right x) (Just y) = x `shouldBe` y

spec :: Spec
spec = do
  describe "decls" $ do
    it "parses declarations" $ do
      parseWhole decls "x = 3"         `matchRight` Just [VarDecl "x" $ int 3]
      parseWhole decls "x = 3;y = 4"   `matchRight` Just [VarDecl "x" $ int 3, VarDecl "y" $ int 4]
      parseWhole decls "x = 3; y = 4;" `matchRight` Just [VarDecl "x" $ int 3, VarDecl "y" $ int 4]

      parseWhole decls "x :: A; x = 3" `matchRight` Just [TypeAnn "x" $ TypeCon "A", VarDecl "x" $ int 3]
      parseWhole decls "x :: A; x = 3; type A = B"
        `matchRight` Just
        [ TypeAnn "x" $ TypeCon "A"
        , VarDecl "x" $ int 3
        , TypeDecl "A" $ TypeCon "B"
        ]
      parseWhole decls "data A = B C | D | E (F -> G -> {}) ()"
        `matchRight` Just
        [ DataDecl "A"
          [ ("B", [TypeCon "C"])
          , ("D", [])
          , ("E", [foldr1 fn [TypeCon "F", TypeCon "G", tRecordN []], tUnit])
          ]
        ]

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

      parseExpr "(1, 2)"         `shouldSatisfy` rightIs (Tuple [int 1, int 2])
      parseExpr "{a = 1, b = 2}" `shouldSatisfy` rightIs (Record [("a", int 1), ("b", int 2)])

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

    it "parses binary operations if any of them are not reserved" $ do
      parseExpr "1 @ 2"   `shouldSatisfy` isLeft
      parseExpr "1 @@ 2 " `shouldSatisfy` rightIs (BinOp "@@" (int 1) (int 2))

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

  describe "varDecl" $ do
    it "parses a variable declaration" $ do
      parseWhole varDecl "x = 12"            `shouldSatisfy` rightIs ("x", int 12)
      parseWhole varDecl "f = \\x -> x + 12" `shouldSatisfy` rightIs ("f", Abs "x" . BinOp "+" (Var "x") $ int 12)

  describe "parseTypeAnn" $
    it "parses a type annotation declaration" $ do
      let var = TypeVar
          con = TypeCon
          conI = con "Int"
          conB = con "Bool"
          conC = con "Char"

      parseWhole parseTypeAnn "i :: Int"                 `shouldSatisfy` rightIs (TypeAnn "i" conI)
      parseWhole parseTypeAnn "f :: Bool -> Char"        `shouldSatisfy` rightIs (TypeAnn "f" $ fn conB conC)
      parseWhole parseTypeAnn "f::Bool->Char"            `shouldSatisfy` rightIs (TypeAnn "f" $ fn conB conC)
      parseWhole parseTypeAnn "g :: Int -> Bool -> Char" `shouldSatisfy` rightIs (TypeAnn "g" . fn conI $ fn conB conC)
      parseWhole parseTypeAnn "g::Int ->  Bool  -> Char" `shouldSatisfy` rightIs (TypeAnn "g" . fn conI $ fn conB conC)

      parseWhole parseTypeAnn "x :: a"      `shouldSatisfy` rightIs (TypeAnn "x" $ var "a")
      parseWhole parseTypeAnn "x :: a -> a" `shouldSatisfy` rightIs (TypeAnn "x" $ var "a" `fn` var "a")

  describe "parseTypeDecl" $ do
    it "parses a type synonym" $ do
      let var = TypeVar
          con = TypeCon
          conI = con "Int"
          conB = con "Bool"
          conC = con "Char"

      parseWhole parseTypeDecl "type I = Int"                `shouldSatisfy` rightIs (TypeDecl "I" $ con "Int")
      parseWhole parseTypeDecl "type Fn = Bool -> Char"      `shouldSatisfy` rightIs (TypeDecl "Fn" $ con "Bool" `fn` con "Char")
      parseWhole parseTypeDecl "type B = Int -> Int -> Bool" `shouldSatisfy` rightIs (TypeDecl "B" . fn (con "Int") $ con "Int" `fn` con "Bool")
      parseWhole parseTypeDecl "type B=Int -> Int -> Bool"   `shouldSatisfy` rightIs (TypeDecl "B" . fn (con "Int") $ con "Int" `fn` con "Bool")

    context "when there are no spaces between 'type' keyword and the type identifier" $
      it "fails" $
        parseWhole parseTypeDecl "typeI = Int" `shouldSatisfy` isLeft

  describe "decl" $
    it "parses a declaration" $ do
      let con = TypeCon

      parseWhole decl "x = 2"        `shouldSatisfy` rightIs (VarDecl "x" $ int 2)
      parseWhole decl "x :: Int"     `shouldSatisfy` rightIs (TypeAnn "x" $ con "Int")
      parseWhole decl "type I = Int" `shouldSatisfy` rightIs (TypeDecl "I" $ con "Int")
      parseWhole decl "data I = A T" `shouldSatisfy` rightIs (DataDecl "I" [("A", [con "T"])])

  describe "emptyList" $
    it "parses a empty list" $ do
      parseWhole emptyList "[]"  `shouldSatisfy` rightIs LitEmptyList
      parseWhole emptyList "[ ]" `shouldSatisfy` rightIs LitEmptyList

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

  describe "tuple" $ do
    it "parses a tuple" $ do
      parseWhole tuple "(1,2)"     `shouldSatisfy` rightIs (Tuple [int 1, int 2])
      parseWhole tuple "(1, 2)"    `shouldSatisfy` rightIs (Tuple [int 1, int 2])
      parseWhole tuple "( 1 , 2 )" `shouldSatisfy` rightIs (Tuple [int 1, int 2])

      parseWhole tuple "(1, 2, 4)"           `shouldSatisfy` rightIs (Tuple [int 1, int 2, int 4])
      parseWhole tuple "(True, 3, 2, False)" `shouldSatisfy` rightIs (Tuple [bool True, int 3, int 2, bool False])

    it "can parse a nested tuple" $ do
      parseWhole tuple "((1, 3), True)"               `shouldSatisfy` rightIs (Tuple [Tuple [int 1, int 3], bool True])
      parseWhole tuple "((False, 3), (2, True))"      `shouldSatisfy` rightIs (Tuple [Tuple [bool False, int 3], Tuple [int 2, bool True]])
      parseWhole tuple "((1, 3), True, (1, 2, 3, 4))" `shouldSatisfy` rightIs (Tuple [Tuple [int 1, int 3], bool True, Tuple [int 1, int 2, int 3, int 4]])

    it "fails if given no tuple (n >= 2)" $ do
      parseWhole tuple "()"   `shouldSatisfy` isLeft
      parseWhole tuple "(1)"  `shouldSatisfy` isLeft
      parseWhole tuple "(1,)" `shouldSatisfy` isLeft

  describe "parsePAs" $
    it "parses as-pattern" $ do
      parseWhole parsePAs "x @ 1"  `shouldSatisfy` rightIs (PAs "x" . PLit $ LitInt 1)
      parseWhole parsePAs "y' @ _" `shouldSatisfy` rightIs (PAs "y'" PWildcard)

      parseWhole parsePAs "x@1" `shouldSatisfy` rightIs (PAs "x" . PLit $ LitInt 1)

  describe "parsePat" $
    it "parses a pattern" $ do
      parseWhole parsePat "_"     `shouldSatisfy` rightIs PWildcard
      parseWhole parsePat "x"     `shouldSatisfy` rightIs (PVar "x")
      parseWhole parsePat "0"     `shouldSatisfy` rightIs (PLit $ LitInt 0)
      parseWhole parsePat "A"     `shouldSatisfy` rightIs (PCon "A" [])
      parseWhole parsePat "A 1 2" `shouldSatisfy` rightIs (PCon "A" [PLit $ LitInt 1, PLit $ LitInt 2])

      parseWhole parsePat "x @ 1"      `shouldSatisfy` rightIs (PAs "x" $ PLit $ LitInt 1)
      parseWhole parsePat "x @ (1, 2)" `shouldSatisfy` rightIs (PAs "x" $ PCon "(,2)" [PLit $ LitInt 1, PLit $ LitInt 2])

  describe "dataDecl" $ do
    let var = TypeVar
        con = TypeCon

    it "parses a datatype declaration" $ do
      parseWhole dataDecl "data A = A Int" `shouldSatisfy` rightIs (DataDecl "A" [("A", [(con "Int")])])
      parseWhole dataDecl "data A = B Int" `shouldSatisfy` rightIs (DataDecl "A" [("B", [(con "Int")])])

      parseWhole dataDecl "data A=B Int" `shouldSatisfy` rightIs (DataDecl "A" [("B", [(con "Int")])])

      parseWhole dataDecl "data A = B" `shouldSatisfy` rightIs (DataDecl "A" [("B", [])])

      parseWhole dataDecl "data A = B ()"   `shouldSatisfy` rightIs (DataDecl "A" [("B", [tUnit])])
      parseWhole dataDecl "data A = B (())" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tUnit])])

      parseWhole dataDecl "data A = B (Int, Bool)"   `shouldSatisfy` rightIs (DataDecl "A" [("B", [tTupleN 2 `TypeApp` (con "Int") `TypeApp` (con "Bool")])])
      parseWhole dataDecl "data A = B ((Int), Bool)" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tTupleN 2 `TypeApp` (con "Int") `TypeApp` (con "Bool")])])
      parseWhole dataDecl "data A = B (Int, ())"     `shouldSatisfy` rightIs (DataDecl "A" [("B", [tTupleN 2 `TypeApp` (con "Int") `TypeApp` tUnit])])

      parseWhole dataDecl "data A = B ((), (), (Int, Bool))"
        `shouldSatisfy`
        rightIs (DataDecl "A" [("B", [tTupleN 3 `TypeApp` tUnit `TypeApp` tUnit `TypeApp` (tTupleN 2 `TypeApp` (con "Int") `TypeApp` (con "Bool"))])])

      parseWhole dataDecl "data A = B {}" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN []])])
      parseWhole dataDecl "data A = B{}"  `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN []])])

      parseWhole dataDecl "data A = B {a = Int}"           `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["a"] `TypeApp` (con "Int")])])
      parseWhole dataDecl "data A = B {a = Bool, b = Int}" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["a", "b"] `TypeApp` (con "Bool") `TypeApp` (con "Int")])])
      parseWhole dataDecl "data A = B {b = Int, a = Bool}" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["b", "a"] `TypeApp` (con "Int") `TypeApp` (con "Bool")])])
      parseWhole dataDecl "data A = B {a = Int, a = Bool}" `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["a", "a"] `TypeApp` (con "Int") `TypeApp` (con "Bool")])])
      parseWhole dataDecl "data A = B {a = Int, a = Int}"  `shouldSatisfy` rightIs (DataDecl "A" [("B", [tRecordN ["a", "a"] `TypeApp` (con "Int") `TypeApp` (con "Int")])])

      parseWhole dataDecl "data A = B Int Bool"      `shouldSatisfy` rightIs (DataDecl "A" [("B", [(con "Int"), (con "Bool")])])
      parseWhole dataDecl "data A = B (Int -> Bool)" `shouldSatisfy` rightIs (DataDecl "A" [("B", [(con "Int") `fn` (con "Bool")])])

    it "parses a variant declaration" $ do
      parseWhole dataDecl "data A = B | C"     `shouldSatisfy` rightIs (DataDecl "A" [("B", []), ("C", [])])
      parseWhole dataDecl "data A=B|C"         `shouldSatisfy` rightIs (DataDecl "A" [("B", []), ("C", [])])
      parseWhole dataDecl "data A = B | C | D" `shouldSatisfy` rightIs (DataDecl "A" [("B", []), ("C", []), ("D", [])])

      -- FIXME
      parseWhole dataDecl "data A = B Int | C Bool" `shouldSatisfy` rightIs (DataDecl "A" [("B", [(con "Int")]), ("C", [(con "Bool")])])
      parseWhole dataDecl "data A = B Int | C"      `shouldSatisfy` rightIs (DataDecl "A" [("B", [(con "Int")]), ("C", [])])
      parseWhole dataDecl "data A = B Int|C"        `shouldSatisfy` rightIs (DataDecl "A" [("B", [(con "Int")]), ("C", [])])

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
      let var = TypeVar
          con = TypeCon

      parseWhole typeApp "a" `shouldSatisfy` rightIs (var "a")
      parseWhole typeApp "A" `shouldSatisfy` rightIs (con "A")

      parseWhole typeApp "a b" `shouldSatisfy` rightIs (var "a" `TypeApp` var "b")
      parseWhole typeApp "A B" `shouldSatisfy` rightIs (con "A" `TypeApp` con "B")

      parseWhole typeApp "a b c" `shouldSatisfy` rightIs (var "a" `TypeApp` var "b" `TypeApp` var "c")
      parseWhole typeApp "A B C" `shouldSatisfy` rightIs (con "A" `TypeApp` con "B" `TypeApp` con "C")

      parseWhole typeApp "ab X ab bc a A" `shouldSatisfy` (rightIs . foldl1 TypeApp) [var "ab", con "X", var "ab", var "bc", var "a", con "A"]

  describe "typeFn" $ do
    let var = TypeVar
        con = TypeCon

    it "parses a type which may consist of functions" $ do
      parseWhole typeFn "A"      `shouldSatisfy` rightIs (con "A")
      parseWhole typeFn "A -> B" `shouldSatisfy` rightIs (con "A" `fn` con "B")

      parseWhole typeFn "a b"      `shouldSatisfy` rightIs (var "a" `TypeApp` var "b")
      parseWhole typeFn "a b -> C" `shouldSatisfy` rightIs (var "a" `TypeApp` var "b" `fn` con "C")

      parseWhole typeFn "A B C"            `shouldSatisfy` rightIs (con "A" `TypeApp` con "B" `TypeApp` con "C")
      parseWhole typeFn "A B C -> BC cC A" `shouldSatisfy` rightIs (foldl1 TypeApp [con "A", con "B", con "C"] `fn` foldl1 TypeApp [con "BC", var "cC", con "A"])

      parseWhole typeFn "a (b -> C)" `shouldSatisfy` rightIs (TypeApp (var "a") $ var "b" `fn` con "C")

      parseWhole typeFn "a b c"   `shouldSatisfy` rightIs (var "a" `TypeApp` var "b" `TypeApp` var "c")
      parseWhole typeFn "a (b c)" `shouldSatisfy` rightIs (TypeApp (var "a") $ var "b" `TypeApp` var "c")

      parseWhole typeFn "a (b -> C) -> aaaXXX" `shouldSatisfy` rightIs (TypeApp (var "a") (var "b" `fn` con "C") `fn` var "aaaXXX")

    it "parses some primitve type literals" $ do
      parseWhole typeFn "()"     `shouldSatisfy` rightIs tUnit
      parseWhole typeFn "(())"   `shouldSatisfy` rightIs tUnit
      parseWhole typeFn "((()))" `shouldSatisfy` rightIs tUnit

      parseWhole typeFn "() ()" `shouldSatisfy` rightIs (tUnit `TypeApp` tUnit)

      parseWhole typeFn "() -> ()"         `shouldSatisfy` rightIs (tUnit `fn` tUnit)
      parseWhole typeFn "() -> () -> ()"   `shouldSatisfy` rightIs (tUnit `fn` (tUnit `fn` tUnit))
      parseWhole typeFn "() -> (() -> ())" `shouldSatisfy` rightIs (tUnit `fn` (tUnit `fn` tUnit))
      parseWhole typeFn "(() -> ()) -> ()" `shouldSatisfy` rightIs (tUnit `fn` tUnit `fn` tUnit)

      parseWhole typeFn "(a, b)"    `shouldSatisfy` rightIs (tTupleN 2 `TypeApp` var "a" `TypeApp` var "b")
      parseWhole typeFn "(a, b, c)" `shouldSatisfy` rightIs (tTupleN 3 `TypeApp` var "a" `TypeApp` var "b" `TypeApp` var "c")

  describe "typeTerm" $
    it "parses a type term" $ do
      let var = TypeVar
          con = TypeCon

      parseWhole typeTerm "A"   `shouldSatisfy` rightIs (con "A")
      parseWhole typeTerm "(A)" `shouldSatisfy` rightIs (con "A")

      parseWhole typeTerm "(A -> B)"       `shouldSatisfy` rightIs (con "A" `fn` con "B")
      parseWhole typeTerm "((A) -> (B))"   `shouldSatisfy` rightIs (con "A" `fn` con "B")
      parseWhole typeTerm "(((A) -> (B)))" `shouldSatisfy` rightIs (con "A" `fn` con "B")

      parseWhole typeTerm "(a b)" `shouldSatisfy` rightIs (var "a" `TypeApp` var "b")

      parseWhole typeTerm "(a b -> C)"   `shouldSatisfy` rightIs (var "a" `TypeApp` var "b" `fn` con "C")
      parseWhole typeTerm "(a (b -> C))" `shouldSatisfy` rightIs (TypeApp (var "a") $ var "b" `fn` con "C")

      parseWhole typeTerm "(A B C)"   `shouldSatisfy` rightIs (foldl1 TypeApp [con "A", con "B", con "C"])
      parseWhole typeTerm "((A B) C)" `shouldSatisfy` rightIs (foldl1 TypeApp [con "A", con "B", con "C"])
      parseWhole typeTerm "(A (B C))" `shouldSatisfy` rightIs (foldr1 TypeApp [con "A", con "B", con "C"])

      parseWhole typeTerm "(A B C -> BC cC A)"   `shouldSatisfy` rightIs (foldl1 TypeApp [con "A", con "B", con "C"] `fn` foldl1 TypeApp [con "BC", var "cC", con "A"])
      parseWhole typeTerm "(A (B C -> BC) cC A)" `shouldSatisfy` (rightIs . foldl1 TypeApp) [con "A", con "B" `TypeApp` con "C" `fn` con "BC", var "cC", con "A"]

  describe "labeledType" $
    it "parses a labeled type" $ do
      parseWhole labeledType "A B"   `shouldSatisfy` rightIs ("A", [TypeCon "B"])
      parseWhole labeledType "A B C" `shouldSatisfy` rightIs ("A", [TypeCon "B", TypeCon "C"])
      parseWhole labeledType "A a"   `shouldSatisfy` rightIs ("A", [TypeVar "a"])

  describe "variantType" $
    it "parses a variant type" $ do
      parseWhole variantType "A B"       `shouldSatisfy` rightIs [("A", [TypeCon "B"])]
      parseWhole variantType "A B | C D" `shouldSatisfy` rightIs [("A", [TypeCon "B"]), ("C", [TypeCon "D"])]

  describe "record" $ do
    it "parses a record" $ do
      parseWhole record "{}"             `shouldSatisfy` (rightIs . Record) []
      parseWhole record "{x = 3}"        `shouldSatisfy` (rightIs . Record) [("x", int 3)]
      parseWhole record "{x = 3, y = 7}" `shouldSatisfy` (rightIs . Record) [("x", int 3), ("y", int 7)]

      parseWhole record "{x=3,y=7}"           `shouldSatisfy` (rightIs . Record) [("x", int 3), ("y", int 7)]
      parseWhole record "{  x = 3 , y = 7  }" `shouldSatisfy` (rightIs . Record) [("x", int 3), ("y", int 7)]

      parseWhole record "{a = (1, 2), y = 7}" `shouldSatisfy` (rightIs . Record) [("a", Tuple [int 1, int 2]), ("y", int 7)]

      parseWhole record "{a = {a = 1, b = 2}, y = 7}" `shouldSatisfy` (rightIs . Record) [("a", Record [("a", int 1), ("b", int 2)]), ("y", int 7)]

    it "fails if it is invalid syntax" $ do
      parseWhole record "{"       `shouldSatisfy` isLeft
      parseWhole record "}"       `shouldSatisfy` isLeft
      parseWhole record "{a}"     `shouldSatisfy` isLeft
      parseWhole record "{a 2}"   `shouldSatisfy` isLeft
      parseWhole record "{3 = a}" `shouldSatisfy` isLeft
      parseWhole record "{A = 3}" `shouldSatisfy` isLeft

  describe "recordTypeR" $
    it "parses a record type" $ do
      parseWhole recordTypeR "{}"             `shouldSatisfy` rightIs (tRecordN [])
      parseWhole recordTypeR "{a = A}"        `shouldSatisfy` rightIs (tRecordN ["a"] `TypeApp` TypeCon "A")
      parseWhole recordTypeR "{a = A, b = B}" `shouldSatisfy` rightIs (tRecordN ["a", "b"] `TypeApp` TypeCon "A" `TypeApp` TypeCon "B")
