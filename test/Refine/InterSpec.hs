module Refine.InterSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Refine.Inter
import Refine.Parse

spec :: Spec
spec =
  describe "typeOf" $
    it "gets the type of an expression" $ do
      (evalEnv Map.empty . typeOf . Lit . LitInt) 3 `shouldBe` Right TypeInt
      (evalEnv Map.empty . typeOf . Lit . LitBool) False `shouldBe` Right TypeBool
      (evalEnv Map.empty . typeOf . Lit . LitChar) 'a' `shouldBe` Right TypeChar
      (evalEnv Map.empty . typeOf . Lit . LitString) "ab" `shouldBe` Right TypeString

      -- (flip evalState (Map.singleton "op" . ####function####) . runEnv . typeOf . BinOp "op" (Lit $ LitInt 2) . Lit . LitBool) True `shouldBe` TypeFun TypeInt TypeBool
