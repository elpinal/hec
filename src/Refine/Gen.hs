module Refine.Gen where

import Control.Monad.Reader

import qualified Refine.Asm as Asm
import Refine.Inter

type Machine = Reader Int

-- | Runs a @Machine@ with the number of regesters.
runMachine :: Machine a -> Int -> a
runMachine m n = runReader m n

gen :: Address -> ThreeAddress -> Machine Asm.Asm
gen = undefined
