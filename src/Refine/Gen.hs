module Refine.Gen where

import Control.Monad.Reader

import qualified Refine.Asm as Asm
import Refine.Inter

type Machine = Reader Int

-- | Runs a @Machine@ with the number of regesters.
runMachine :: Machine a -> Int -> a
runMachine m n = runReader m n

gen :: Address -> [ThreeAddress] -> Machine [Asm.Inst]
gen (Const c) _ = return [genConst c]
gen _ _ = undefined

genConst :: Constant -> Asm.Inst
genConst (CInt c) = Asm.Ret . Asm.Const $ Asm.CInt c
genConst (CBool b) = Asm.Ret . Asm.Const $ Asm.CBool b
