module Refine.InterSpec where

import Test.Hspec

import Data.Either
import qualified Data.Map.Lazy as Map

import Refine.Inter
import Refine.Parse

spec :: Spec
spec =
  describe "typeOf" $ do
    it "gets the type of an expression" $ do
      (evalEnv interState . typeOf . Lit . LitInt) 3 `shouldBe` Right TypeInt
      (evalEnv interState . typeOf . Lit . LitBool) False `shouldBe` Right TypeBool
      (evalEnv interState . typeOf . Lit . LitChar) 'a' `shouldBe` Right TypeChar
      (evalEnv interState . typeOf . Lit . LitString) "ab" `shouldBe` Right TypeString

      let st = interState { table = Map.singleton "op" . TypeFun TypeInt $ TypeFun TypeBool TypeBool }
      (evalEnv st . typeOf . BinOp "op" (Lit $ LitInt 2) . Lit . LitBool) True `shouldBe` Right TypeBool

      (evalEnv interState . typeOf . Abs "x" . Var) "x" `shouldBe` Right (TypeFun (TypeVar "a0") $ TypeVar "a0")

    it "fails if not typable" $ do
      let st = interState { table = Map.singleton "op" . TypeFun TypeInt $ TypeFun TypeInt TypeBool }
      (evalEnv st . typeOf . BinOp "op" (Lit $ LitInt 2) . Lit . LitBool) True `shouldSatisfy` isLeft
