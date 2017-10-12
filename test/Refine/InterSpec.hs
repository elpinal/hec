module Refine.InterSpec where

import Test.Hspec

import Data.Either
import qualified Data.Map.Lazy as Map

import Refine.Inter
import Refine.Parse

spec :: Spec
spec = do
  describe "typeOf" $ do
    it "gets the type of an expression" $ do
      (evalEnv interState . typeOf . Lit . LitInt) 3 `shouldBe` Right TypeInt
      (evalEnv interState . typeOf . Lit . LitBool) False `shouldBe` Right TypeBool
      (evalEnv interState . typeOf . Lit . LitChar) 'a' `shouldBe` Right TypeChar
      (evalEnv interState . typeOf . Lit . LitString) "ab" `shouldBe` Right TypeString

      let st = interState { table = Map.singleton "op" (TypeFun TypeInt $ TypeFun TypeBool TypeBool, Nothing) }
      (evalEnv st . typeOf . BinOp "op" (Lit $ LitInt 2) . Lit . LitBool) True `shouldBe` Right TypeBool

      (evalEnv interState . typeOf . Abs "x" . Var) "x" `shouldBe` Right (TypeFun (TypeVar "a0") $ TypeVar "a0")

      -- (evalEnv interState . typeOf . Abs "f" . App (Var "f") . Lit . LitInt) 3 `shouldBe` Right (TypeFun (TypeFun TypeInt (TypeVar "a0")) $ TypeVar "a0")

    it "fails if not typable" $ do
      let st = interState { table = Map.singleton "op" (TypeFun TypeInt $ TypeFun TypeInt TypeBool, Nothing) }
      (evalEnv st . typeOf . BinOp "op" (Lit $ LitInt 2) . Lit . LitBool) True `shouldSatisfy` isLeft

      (evalEnv interState . typeOf . App (Lit $ LitInt 1) . Lit . LitInt) 2 `shouldSatisfy` isLeft

  describe "recons" $ do
    it "reconstructs binary operations with fixity definitions" $ do
      (evalEnv interState . recons . Lit . LitInt) 2 `shouldBe` Right (Lit $ LitInt 2)

      let lhs = BinOp "op" (Lit $ LitInt 3) . Lit $ LitInt 4
      let rhs = Lit $ LitInt 4
      let st = interState { table = Map.singleton "op" (TypeFun TypeInt $ TypeFun TypeInt TypeInt, Nothing) }
      (evalEnv st . recons $ BinOp "op" lhs rhs) `shouldBe` Right (BinOp "op" lhs rhs)

      let st = interState { table = Map.singleton "op" (TypeFun TypeInt $ TypeFun TypeInt TypeInt, Just $ Fixity (Just LeftAssoc) 9) }
      (evalEnv st . recons $ BinOp "op" lhs rhs) `shouldBe` Right (BinOp "op" lhs rhs)

      let st = interState { table = Map.singleton "op" (TypeFun TypeInt $ TypeFun TypeInt TypeInt, Just $ Fixity (Just RightAssoc) 9) }
      (evalEnv st . recons $ BinOp "op" lhs rhs) `shouldBe` Right (BinOp "op" (Lit $ LitInt 3) $ BinOp "op" (Lit $ LitInt 4) rhs)

  describe "apply" $ do
    it "substitutes type variables in types" $ do
      Map.empty                    `apply` TypeInt       `shouldBe` TypeInt
      Map.singleton "aaa" TypeBool `apply` TypeInt       `shouldBe` TypeInt
      Map.empty                    `apply` TypeVar "aaa" `shouldBe` TypeVar "aaa"
      Map.singleton "aaa" TypeBool `apply` TypeVar "aaa" `shouldBe` TypeBool
