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
gen' (BinAssign dst op src1 src2) = do
  r1 <- newRegister
  r2 <- newRegister
  load r1 src1
  load r2 src2
  case op of
    Bin "-" -> tell [Asm.ISub r1 (Asm.Reg r1) (Asm.Reg r2)]
gen' _ = undefined

newRegister :: Machine Asm.Register
newRegister = undefined

load :: Asm.Register -> Address -> Machine ()
load = undefined
