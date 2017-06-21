module ParserSpec where

import Test.Hspec

import Safe

import qualified Data.Map.Lazy as Map

import Parser
import Scanner
import qualified Inter

spec :: Spec
spec = do
  describe "parse" $ do
    it "parse" $
      parse (extend testGrammar) [Token (Num,"1"), Token (Sub, "-"), Token (Num, "22")]
      `shouldBe`
      [ (Inter.Point 2, Inter.Arith Inter.Sub, Inter.At 1, Inter.Const 22)
      , (Inter.Point 1, Inter.NOP, Inter.Const 1, Inter.Nil)
      ]

testGrammar :: Grammar
testGrammar = Grammar (Var "expr") $ Map.fromList
  [ "expr" >:> [ refer "expr", Term Add, Term Num] ||| (\xs -> (Inter.Arith Inter.Add, (xs`at`0), (xs`at`2)))
  , "expr" >:> [ refer "expr", Term Sub, Term Num] ||| (\xs -> (Inter.Arith Inter.Sub, (xs`at`0), (xs`at`2)))
  , "expr" >:> [ Term Num ] ||| (\xs -> (Inter.NOP, xs`at`0, Inter.Nil))
  ]
