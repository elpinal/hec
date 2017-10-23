module Refine.TypeSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Refine.Kind
import Refine.Type

spec :: Spec
spec = do
  describe "apply" $
    it "substitutes type variables in types" $ do
      Map.empty                             `apply` tInt                       `shouldBe` tInt
      Map.singleton (TVar "aaa" Star) tBool `apply` tInt                       `shouldBe` tInt
      Map.empty                             `apply` TypeVar1 (TVar "aaa" Star) `shouldBe` TypeVar1 (TVar "aaa" Star)
      Map.singleton (TVar "aaa" Star) tBool `apply` TypeVar1 (TVar "aaa" Star) `shouldBe` tBool

      Map.empty                             `apply` TypeApp (TypeVar1 (TVar "aaa" Star)) (TypeVar1 (TVar "bbb" Star)) `shouldBe` TypeApp (TypeVar1 (TVar "aaa" Star)) (TypeVar1 (TVar "bbb" Star))
      Map.singleton (TVar "aaa" Star) tBool `apply` TypeApp (TypeVar1 (TVar "aaa" Star)) (TypeVar1 (TVar "bbb" Star)) `shouldBe` TypeApp tBool (TypeVar1 (TVar "bbb" Star))

  describe "@@" $ do
    it "composes two substitutions" $ do
      Map.empty @@ Map.empty `shouldBe` Map.empty

      let aToInt = Map.fromList [(TVar "a" Star, tInt)]

      Map.empty @@ aToInt `shouldBe` aToInt
      aToInt @@ Map.empty `shouldBe` aToInt
      aToInt @@ aToInt    `shouldBe` aToInt

      let bToInt = Map.fromList [(TVar "b" Star, tInt)]

      aToInt @@ bToInt `shouldBe` Map.fromList [(TVar "a" Star, tInt), (TVar "b" Star, tInt)]
      bToInt @@ aToInt `shouldBe` Map.fromList [(TVar "a" Star, tInt), (TVar "b" Star, tInt)]

    it "gives priority to the second argument" $ do
      let aToInt = Map.fromList [(TVar "a" Star, tInt)]
          aToChar = Map.fromList [(TVar "a" Star, tChar)]

      aToInt @@ aToChar `shouldBe` aToChar
