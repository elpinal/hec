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
      Map.empty                             `apply` TypeVar (TVar "aaa" Star) `shouldBe` TypeVar (TVar "aaa" Star)
      Map.singleton (TVar "aaa" Star) tBool `apply` TypeVar (TVar "aaa" Star) `shouldBe` tBool

      Map.empty                             `apply` TypeApp (TypeVar (TVar "aaa" Star)) (TypeVar (TVar "bbb" Star)) `shouldBe` TypeApp (TypeVar (TVar "aaa" Star)) (TypeVar (TVar "bbb" Star))
      Map.singleton (TVar "aaa" Star) tBool `apply` TypeApp (TypeVar (TVar "aaa" Star)) (TypeVar (TVar "bbb" Star)) `shouldBe` TypeApp tBool (TypeVar (TVar "bbb" Star))

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

      let aToB = Map.singleton (TVar "a" Star) (TypeVar $ TVar "b" Star)

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
      let aToB = Map.singleton (TVar "a" Star) (TypeVar $ TVar "b" Star)

      bToInt `merge` aToB `shouldBe` Just (Map.union bToInt aToB)
      aToB `merge` bToInt `shouldBe` Just (Map.union bToInt aToB)

  describe "mgu" $ do
    it "obtains the most general unifier of two types" $ do
      tInt `mgu` tInt `shouldBe` Just Map.empty

      tInt `fn` tChar `mgu` fn (TypeVar $ TVar "a" Star) tChar `shouldBe` Just aToInt
      tInt `fn` tChar `mgu` (TypeVar $ TVar "a" Star)          `shouldBe` Just (Map.singleton (TVar "a" Star) $ tInt `fn` tChar)
      TypeVar (TVar "a" Star) `mgu` TypeVar (TVar "a" Star)   `shouldBe` Just Map.empty

      TypeVar (TVar "a" Star) `fn` TypeVar (TVar "a" Star) `mgu` (tInt `fn` TypeVar (TVar "b" Star)) `shouldBe` Just (Map.union aToInt bToInt)

    it "fails on some cases" $ do
      tInt `mgu` tChar `shouldBe` Nothing
      TypeVar (TVar "a" Star) `fn` tInt `mgu` TypeVar (TVar "a" Star)   `shouldBe` Nothing
      TypeVar (TVar "a" Star) `mgu` TypeVar (TVar "b" $ KFun Star Star) `shouldBe` Nothing

  describe "match" $ do
    it "finds a substitution s where apply s t1 = t2" $ do
      tInt `match` tInt `shouldBe` Just Map.empty

      TypeVar (TVar "a" Star) `match` TypeVar (TVar "a" Star)   `shouldBe` Just Map.empty

    it "fails on some cases" $ do
      tInt `fn` tChar `match` fn (TypeVar $ TVar "a" Star) tChar `shouldBe` Nothing
      tInt `match` tChar `shouldBe` Nothing

      TypeVar (TVar "a" Star) `fn` tInt `match` TypeVar (TVar "a" Star)   `shouldBe` Nothing
      TypeVar (TVar "a" Star) `match` TypeVar (TVar "b" $ KFun Star Star) `shouldBe` Nothing
      tInt `fn` tChar `match` (TypeVar $ TVar "a" Star)                    `shouldBe` Nothing

      TypeVar (TVar "a" Star) `fn` TypeVar (TVar "a" Star) `match` (tInt `fn` TypeVar (TVar "b" Star)) `shouldBe` Nothing

  describe "addClass" $ do
    it "defines a class" $ do
      let sEnv = emptyEnv { classes = Map.singleton "S" ([], []) }

      classes <$> addClass "A" []    emptyEnv `shouldBe` Just (Map.singleton "A" ([], []))
      classes <$> addClass "A" ["S"]     sEnv `shouldBe` (Just . Map.fromList) [("A", (["S"], [])), ("S", ([], []))]

    it "fails if the class is already defined" $ do
      let aEnv = emptyEnv { classes = Map.singleton "A" ([], []) }
      classes <$> addClass "A" [] aEnv `shouldBe` Nothing

    it "fails if not all the superclasses are defined" $ do
      classes <$> addClass "A" ["S"] emptyEnv `shouldBe` Nothing

  describe "addInst" $
    it "defines a instance for a class with some predicates" $ do
      let aEnv = emptyEnv { classes = Map.singleton "A" ([], []) }
          aVar = TypeVar $ TVar "a" Star

      classes <$> addInst []              (IsIn "A" tInt)        aEnv `shouldBe` (Just . Map.singleton "A") ([], [[] :=> IsIn "A" tInt])
      classes <$> addInst [IsIn "A" aVar] (IsIn "A" $ list aVar) aEnv `shouldBe` (Just . Map.singleton "A") ([], [[IsIn "A" aVar] :=> IsIn "A" (list aVar)])

  describe "overlap" $
    it "checks overlap" $
      overlap (IsIn "A" tInt) (IsIn "A" tInt) `shouldBe` True

aToInt :: Subst
aToInt = Map.fromList [(TVar "a" Star, tInt)]

bToInt :: Subst
bToInt = Map.fromList [(TVar "b" Star, tInt)]

emptyEnv :: ClassEnv
emptyEnv = ClassEnv { classes = Map.empty, defaults = [] }
