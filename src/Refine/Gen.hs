module Refine.Gen where

import Control.Monad.Reader
import Control.Monad.Writer.Lazy

import qualified Refine.Asm as Asm
import Refine.Inter

type Machine = WriterT [Asm.Inst] (Reader Int)

-- | Runs a @Machine@ with the number of regesters.
runMachine :: Machine a -> Int -> (a, [Asm.Inst])
runMachine m n = runReader (runWriterT m) n

gen :: Address -> [ThreeAddress] -> Machine Asm.Operand
gen (Const c) _ = return $ genConst c
gen (Name s) xs = mapM_ gen' xs >> return (Asm.LValue s)
gen _ _ = undefined

genConst :: Constant -> Asm.Operand
genConst (CInt c) = Asm.Const $ Asm.CInt c
genConst (CBool b) = Asm.Const $ Asm.CBool b

gen' :: ThreeAddress -> Machine ()
gen' = undefined
