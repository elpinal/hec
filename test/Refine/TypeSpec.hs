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

      Map.empty @@ aToInt `shouldBe` aToInt
      aToInt @@ Map.empty `shouldBe` aToInt
      aToInt @@ aToInt    `shouldBe` aToInt

      aToInt @@ bToInt `shouldBe` Map.fromList [(TVar "a" Star, tInt), (TVar "b" Star, tInt)]
      bToInt @@ aToInt `shouldBe` Map.fromList [(TVar "a" Star, tInt), (TVar "b" Star, tInt)]

    it "gives priority to the second argument" $ do
      let aToChar = Map.fromList [(TVar "a" Star, tChar)]

      aToInt @@ aToChar `shouldBe` aToChar

      let aToB = Map.singleton (TVar "a" Star) (TypeVar1 $ TVar "b" Star)

      bToInt @@ aToB `shouldBe` Map.union aToInt bToInt
      aToB @@ bToInt `shouldBe` Map.union aToB bToInt

  describe "merge" $ do
    it "composes two substitutions parallel" $ do
      Map.empty `merge` Map.empty `shouldBe` Just Map.empty

      Map.empty `merge` aToInt `shouldBe` Just aToInt
      aToInt `merge` Map.empty `shouldBe` Just aToInt
      aToInt `merge` aToInt    `shouldBe` Just aToInt

      aToInt `merge` bToInt `shouldBe` (Just . Map.fromList) [(TVar "a" Star, tInt), (TVar "b" Star, tInt)]
      bToInt `merge` aToInt `shouldBe` (Just . Map.fromList) [(TVar "a" Star, tInt), (TVar "b" Star, tInt)]

    it "fails when one or more mappings conflict" $ do
      let aToChar = Map.fromList [(TVar "a" Star, tChar)]

      aToInt `merge` aToChar `shouldBe` Nothing

    it "is symmetric if success" $ do
      let aToB = Map.singleton (TVar "a" Star) (TypeVar1 $ TVar "b" Star)

      bToInt `merge` aToB `shouldBe` Just (Map.union bToInt aToB)
      aToB `merge` bToInt `shouldBe` Just (Map.union bToInt aToB)

  describe "mgu" $ do
    it "obtains the most general unifier of two types" $ do
      tInt `mgu` tInt `shouldBe` Just Map.empty

      tInt `fn` tChar `mgu` fn (TypeVar1 $ TVar "a" Star) tChar `shouldBe` Just aToInt
      tInt `fn` tChar `mgu` (TypeVar1 $ TVar "a" Star)          `shouldBe` Just (Map.singleton (TVar "a" Star) $ tInt `fn` tChar)
      TypeVar1 (TVar "a" Star) `mgu` TypeVar1 (TVar "a" Star)   `shouldBe` Just Map.empty

      TypeVar1 (TVar "a" Star) `fn` TypeVar1 (TVar "a" Star) `mgu` (tInt `fn` TypeVar1 (TVar "b" Star)) `shouldBe` Just (Map.union aToInt bToInt)

    it "fails on some cases" $ do
      tInt `mgu` tChar `shouldBe` Nothing
      TypeVar1 (TVar "a" Star) `fn` tInt `mgu` TypeVar1 (TVar "a" Star)   `shouldBe` Nothing
      TypeVar1 (TVar "a" Star) `mgu` TypeVar1 (TVar "b" $ KFun Star Star) `shouldBe` Nothing

  describe "match" $ do
    it "finds a substitution s where apply s t1 = t2" $ do
      tInt `match` tInt `shouldBe` Just Map.empty

      TypeVar1 (TVar "a" Star) `match` TypeVar1 (TVar "a" Star)   `shouldBe` Just Map.empty

    it "fails on some cases" $ do
      tInt `fn` tChar `match` fn (TypeVar1 $ TVar "a" Star) tChar `shouldBe` Nothing
      tInt `match` tChar `shouldBe` Nothing

      TypeVar1 (TVar "a" Star) `fn` tInt `match` TypeVar1 (TVar "a" Star)   `shouldBe` Nothing
      TypeVar1 (TVar "a" Star) `match` TypeVar1 (TVar "b" $ KFun Star Star) `shouldBe` Nothing
      tInt `fn` tChar `match` (TypeVar1 $ TVar "a" Star)                    `shouldBe` Nothing

      TypeVar1 (TVar "a" Star) `fn` TypeVar1 (TVar "a" Star) `match` (tInt `fn` TypeVar1 (TVar "b" Star)) `shouldBe` Nothing

aToInt :: Subst
aToInt = Map.fromList [(TVar "a" Star, tInt)]

bToInt :: Subst
bToInt = Map.fromList [(TVar "b" Star, tInt)]
