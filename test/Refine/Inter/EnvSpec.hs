module Refine.Inter.EnvSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Refine.AST
import Refine.Inter.Env
import qualified Refine.Type.Syntactic as S

spec :: Spec
spec = do
  describe "scanDecls" $ do
    it "scans declarations" $ do
      scanDecls []                     `shouldBe` return emptyDecls
      scanDecls [VarDecl "x" $ int 23] `shouldBe` return (updateVars (Map.insert "x" (Just $ int 23, Nothing)) emptyDecls)

      scanDecls [TypeAnn "x" $ S.TypeCon "A"]                       `shouldBe` return (updateVars (Map.insert "x" (Nothing, Just $ S.TypeCon "A")) emptyDecls)
      scanDecls [TypeAnn "x" $ S.TypeCon "A", VarDecl "x" $ int 23] `shouldBe` return (updateVars (Map.insert "x" (Just $ int 23, Just $ S.TypeCon "A")) emptyDecls)

    context "when given duplicated declarations" $
      it "return an error" $ do
        scanDecls [VarDecl "x" $ int 23, VarDecl "x" $ int 0]                `shouldBe` Left (Duplicate "x")
        scanDecls [TypeAnn "x" $ S.TypeCon "A", TypeAnn "x" $ S.TypeVar "a"] `shouldBe` Left (Duplicate "x")
