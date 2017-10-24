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

      let aToB = Map.singleton (TVar "a" Star) (TypeVar1 $ TVar "b" Star)
          bToInt = Map.singleton (TVar "b" Star) tInt

      bToInt @@ aToB `shouldBe` Map.union aToInt bToInt
      aToB @@ bToInt `shouldBe` Map.union aToB bToInt

  describe "merge" $ do
    it "composes two substitutions parallel" $ do
      Map.empty `merge` Map.empty `shouldBe` Just Map.empty

      let aToInt = Map.fromList [(TVar "a" Star, tInt)]

      Map.empty `merge` aToInt `shouldBe` Just aToInt
      aToInt `merge` Map.empty `shouldBe` Just aToInt
      aToInt `merge` aToInt    `shouldBe` Just aToInt

      let bToInt = Map.fromList [(TVar "b" Star, tInt)]

      aToInt `merge` bToInt `shouldBe` (Just . Map.fromList) [(TVar "a" Star, tInt), (TVar "b" Star, tInt)]
      bToInt `merge` aToInt `shouldBe` (Just . Map.fromList) [(TVar "a" Star, tInt), (TVar "b" Star, tInt)]

    it "fails when one or more mappings conflict" $ do
      let aToInt = Map.fromList [(TVar "a" Star, tInt)]
          aToChar = Map.fromList [(TVar "a" Star, tChar)]

      aToInt `merge` aToChar `shouldBe` Nothing

    it "is symmetric if success" $ do
      let aToB = Map.singleton (TVar "a" Star) (TypeVar1 $ TVar "b" Star)
          aToInt = Map.fromList [(TVar "a" Star, tInt)]
          bToInt = Map.singleton (TVar "b" Star) tInt

      bToInt `merge` aToB `shouldBe` Just (Map.union bToInt aToB)
      aToB `merge` bToInt `shouldBe` Just (Map.union bToInt aToB)
