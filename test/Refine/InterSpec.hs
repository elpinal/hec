module Refine.InterSpec where

import Test.Hspec

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

import Refine.Inter
import Refine.Parse

spec :: Spec
spec =
  describe "typeOf" $
    it "gets the type of an expression" $ do
      (flip evalState Map.empty . runExceptT . typeOf . Lit . LitInt) 3 `shouldBe` Right TypeInt
      (flip evalState Map.empty . runExceptT . typeOf . Lit . LitBool) False `shouldBe` Right TypeBool
      (flip evalState Map.empty . runExceptT . typeOf . Lit . LitChar) 'a' `shouldBe` Right TypeChar
      (flip evalState Map.empty . runExceptT . typeOf . Lit . LitString) "ab" `shouldBe` Right TypeString

      -- (flip evalState (Map.singleton "op" . ####function####) . runEnv . typeOf . BinOp "op" (Lit $ LitInt 2) . Lit . LitBool) True `shouldBe` TypeFun TypeInt TypeBool
