module ParserSpec where

import Test.Hspec

import Safe

import qualified Data.Map.Lazy as Map

import Parser
import Scanner
import qualified Inter

spec :: Spec
spec = do
  describe "parse" $
    it "parses tokens into intermediate codes with a grammar" $
      parse testGrammar [createToken "1" Num, createToken "-" Sub, createToken "22" Num]
      `shouldBe`
      [ (Inter.Point 2, Inter.Arith Inter.Sub, Inter.At 1, Inter.Const 22)
      , (Inter.Point 1, Inter.NOP, Inter.Const 1, Inter.Nil)
      ]

  describe "nulls" $ do
    it "returns nothing when empty list given" $
      nulls [] `shouldBe` []

    it "gets a 'null' set" $ do
      nulls ["expr" >:> [refer "expr"]] `shouldBe` []
      nulls ["expr" >:> []] `shouldBe` [Var "expr"]
      nulls [ "expr" >:> [refer "expr"]
            , "expr" >:> [refer "term"]
            , "term" >:> []
            ] `shouldBe` [Var "expr", Var "term"]

testGrammar :: Grammar
testGrammar = extend (Var "expr") $ Map.fromList
  [ "expr" >:> [ refer "expr", Term Add, Term Num] ||| (\xs -> (Inter.Arith Inter.Add, xs `at` 0, xs `at` 2))
  , "expr" >:> [ refer "expr", Term Sub, Term Num] ||| (\xs -> (Inter.Arith Inter.Sub, xs `at` 0, xs `at` 2))
  , "expr" >:> [ Term Num ]                        ||| (\xs -> (Inter.NOP, xs `at` 0, Inter.Nil))
  ]
