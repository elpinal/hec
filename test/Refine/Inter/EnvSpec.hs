module Refine.Inter.EnvSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Refine.AST
import Refine.Inter.Env
import Refine.Kind
import qualified Refine.Type.Syntactic as S

spec :: Spec
spec = do
  describe "scanDecls" $ do
    it "scans declarations" $ do
      scanDecls []                     `shouldBe` return emptyDecls
      scanDecls [VarDecl "x" $ int 23] `shouldBe` return (updateVars (Map.insert "x" (Just $ int 23, Nothing)) emptyDecls)

      scanDecls [TypeAnn "x" $ S.TypeCon "A"]                       `shouldBe` return (updateVars (Map.insert "x" (Nothing, Just $ S.TypeCon "A")) emptyDecls)
      scanDecls [TypeAnn "x" $ S.TypeCon "A", VarDecl "x" $ int 23] `shouldBe` return (updateVars (Map.insert "x" (Just $ int 23, Just $ S.TypeCon "A")) emptyDecls)

      scanDecls [TypeDecl "A" $ S.TypeCon "B"] `shouldBe` return (updateTypes (Map.insert "A" $ S.TypeCon "B") emptyDecls)
      scanDecls [DataDecl "A" $ S.TypeCon "B"] `shouldBe` return (updateTypes (Map.insert "A" $ S.TypeCon "B") emptyDecls)

    context "when given duplicated declarations" $
      it "return an error" $ do
        scanDecls [VarDecl "x" $ int 23, VarDecl "x" $ int 0]                  `shouldBe` Left (Duplicate "x")
        scanDecls [TypeAnn "x" $ S.TypeCon "A", TypeAnn "x" $ S.TypeVar "a"]   `shouldBe` Left (Duplicate "x")
        scanDecls [TypeDecl "A" $ S.TypeCon "B", TypeDecl "A" $ S.TypeCon "C"] `shouldBe` Left (Duplicate "A")

  describe "kindOf" $ do
    it "gets the kind of a type" $ do
      let kind t ts = runKindEnv (kindOf t ts)
      kind (S.TypeCon "A") Map.empty Map.empty                `shouldBe` Left (Undefined "A")
      kind (S.TypeCon "A") Map.empty (Map.singleton "A" Star) `shouldBe` Right Star

      kind (S.TypeCon "A") (Map.singleton "A" $ S.TypeCon "B") (Map.singleton "A" Star) `shouldBe` Left (Undefined "B")
      kind (S.TypeCon "A") (Map.singleton "A" $ S.TypeCon "B") (Map.singleton "B" Star) `shouldBe` Right Star
