module Refine.InterSpec where

import Test.Hspec

import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

import Refine.Inter
import Refine.Parse

spec :: Spec
spec =
  describe "typeOf" $
    it "gets the type of an expression" $ do
      (flip evalState Map.empty . runEnv . typeOf . Lit . LitInt) 3 `shouldBe` TypeInt
      (flip evalState Map.empty . runEnv . typeOf . Lit . LitBool) False `shouldBe` TypeBool
      (flip evalState Map.empty . runEnv . typeOf . Lit . LitChar) 'a' `shouldBe` TypeChar
      (flip evalState Map.empty . runEnv . typeOf . Lit . LitString) "ab" `shouldBe` TypeString

      -- (flip evalState (Map.singleton "op" . ####function####) . runEnv . typeOf . BinOp "op" (Lit $ LitInt 2) . Lit . LitBool) True `shouldBe` TypeFun TypeInt TypeBool
