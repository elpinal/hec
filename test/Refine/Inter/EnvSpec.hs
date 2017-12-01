module Refine.Inter.EnvSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Refine.AST
import Refine.Inter.Env

spec :: Spec
spec = do
  describe "scanDecls" $ do
    it "scans declarations" $ do
      scanDecls []                     `shouldBe` return emptyDecls
      scanDecls [VarDecl "x" $ int 23] `shouldBe` return (updateVars (Map.insert "x" (int 23, Nothing)) emptyDecls)

    context "when given duplicated declarations" $
      it "returns Nothing" $ do
        scanDecls [VarDecl "x" $ int 23, VarDecl "x" $ int 0] `shouldBe` Nothing
