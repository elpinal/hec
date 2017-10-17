module Refine.TypeSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Refine.Kind
import Refine.Type

spec :: Spec
spec =
  describe "apply" $
    it "substitutes type variables in types" $ do
      Map.empty                             `apply` tInt                       `shouldBe` tInt
      Map.singleton (TVar "aaa" Star) tBool `apply` tInt                       `shouldBe` tInt
      Map.empty                             `apply` TypeVar1 (TVar "aaa" Star) `shouldBe` TypeVar1 (TVar "aaa" Star)
      Map.singleton (TVar "aaa" Star) tBool `apply` TypeVar1 (TVar "aaa" Star) `shouldBe` tBool

      Map.empty                             `apply` TypeApp (TypeVar1 (TVar "aaa" Star)) (TypeVar1 (TVar "bbb" Star)) `shouldBe` TypeApp (TypeVar1 (TVar "aaa" Star)) (TypeVar1 (TVar "bbb" Star))
      Map.singleton (TVar "aaa" Star) tBool `apply` TypeApp (TypeVar1 (TVar "aaa" Star)) (TypeVar1 (TVar "bbb" Star)) `shouldBe` TypeApp tBool (TypeVar1 (TVar "bbb" Star))
