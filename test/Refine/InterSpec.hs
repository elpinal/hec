module Refine.InterSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Refine.Inter
import Refine.Parse
import Refine.Type

spec :: Spec
spec = do
  describe "recons" $
    it "reconstructs binary operations with fixity definitions" $ do
      (evalEnv interState . recons . Lit . LitInt) 2 `shouldBe` Right (Lit $ LitInt 2)

      let lhs = BinOp "op" (Lit $ LitInt 3) . Lit $ LitInt 4
      let rhs = Lit $ LitInt 4
      let st = interState { table = Map.singleton "op" (fn tInt $ fn tInt tInt, Nothing) }
      (evalEnv st . recons $ BinOp "op" lhs rhs) `shouldBe` Right (BinOp "op" lhs rhs)

      let st = interState { table = Map.singleton "op" (fn tInt $ fn tInt tInt, Just $ Fixity (Just LeftAssoc) 9) }
      (evalEnv st . recons $ BinOp "op" lhs rhs) `shouldBe` Right (BinOp "op" lhs rhs)

      let st = interState { table = Map.singleton "op" (fn tInt $ fn tInt tInt, Just $ Fixity (Just RightAssoc) 9) }
      (evalEnv st . recons $ BinOp "op" lhs rhs) `shouldBe` Right (BinOp "op" (Lit $ LitInt 3) $ BinOp "op" (Lit $ LitInt 4) rhs)

  describe "binToFun" $
    it "convert binary operations to function applications" $ do
      binToFun (Lit $ LitInt 3) `shouldBe` Lit (LitInt 3)
      binToFun (BinOp "#" (Lit $ LitInt 1) . Lit $ LitInt 3) `shouldBe` (App (App (Var "#") . Lit $ LitInt 1) . Lit $ LitInt 3)
      binToFun (BinOp "@" (Var "x") . App (Abs "n" $ Var "n") $ Var "y") `shouldBe` (App (App (Var "@") $ Var "x") . App (Abs "n" $ Var "n") $ Var "y")

  describe "genThreeAddress" $
    it "generates three address code" $ do
      translate interState (genThreeAddress (Lit $ LitInt 3)) `shouldBe` Right (Const $ CInt 3, [])

      translate interState (genThreeAddress . BinOp "+" (Lit $ LitInt 3) . Lit $ LitInt 4)
        `shouldBe`
        Right (TempVar 0, [BinAssign (TempVar 0) (Bin "+") (Const $ CInt 3) . Const $ CInt 4])

      translate interState (genThreeAddress . App (Abs "x" $ Var "x") . Lit $ LitInt 4)
        `shouldBe`
        -- The wrong value: fix genThreeAddress for Abs.
        Right (TempVar 0, [Begin, Return (Name "x"), End, Param (Const $ CInt 4), Call (TempVar 0) (Label 1)])
