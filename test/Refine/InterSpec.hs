module Refine.InterSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Refine.AST (int)
import Refine.Inter
import Refine.Parse
import Refine.Type

spec :: Spec
spec = do
  describe "recons" $
    it "reconstructs binary operations with fixity definitions" $ do
      (evalEnv interState . recons . int) 2 `shouldBe` Right (int 2)

      let lhs = BinOp "op" (int 3) $ int 4
      let rhs = int 4
      let st = interState { table = Map.singleton "op" (fn tInt $ fn tInt tInt, Nothing) }
      (evalEnv st . recons $ BinOp "op" lhs rhs) `shouldBe` Right (BinOp "op" lhs rhs)

      let st = interState { table = Map.singleton "op" (fn tInt $ fn tInt tInt, Just $ Fixity (Just LeftAssoc) 9) }
      (evalEnv st . recons $ BinOp "op" lhs rhs) `shouldBe` Right (BinOp "op" lhs rhs)

      let st = interState { table = Map.singleton "op" (fn tInt $ fn tInt tInt, Just $ Fixity (Just RightAssoc) 9) }
      (evalEnv st . recons $ BinOp "op" lhs rhs) `shouldBe` Right (BinOp "op" (int 3) $ BinOp "op" (int 4) rhs)

  describe "binToFun" $
    it "convert binary operations to function applications" $ do
      binToFun (int 3) `shouldBe` int 3
      binToFun (BinOp "#" (int 1) $ int 3) `shouldBe` (Var "#" `App` int 1 `App` int 3)

      let app = App (Abs "n" $ Var "n") $ Var "y"
          op = BinOp "@"
          x = Var "x"

      binToFun (x `op` app) `shouldBe` (Var "@" `App` x `App` app)

  describe "genThreeAddress" $
    it "generates three address code" $ do
      translate interState (genThreeAddress (int 3)) `shouldBe` Right (Const $ CInt 3, [])

      translate interState (genThreeAddress . BinOp "+" (int 3) $ int 4)
        `shouldBe`
        Right (TempVar 0, [BinAssign (TempVar 0) (Bin "+") (Const $ CInt 3) . Const $ CInt 4])

      translate interState (genThreeAddress . App (Abs "x" $ Var "x") $ int 4)
        `shouldBe`
        -- The wrong value: fix genThreeAddress for Abs.
        Right (TempVar 0, [Begin, Return (Name "x"), End, Param (Const $ CInt 4), Call (TempVar 0) (Label 1)])
