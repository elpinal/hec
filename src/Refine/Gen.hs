module Refine.Gen where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import qualified Data.Map.Lazy as Map

import qualified Refine.Asm as Asm
import Refine.Inter

type Machine = WriterT [Asm.Inst] (StateT (RegisterDescriptor, AddressDescriptor) (Reader Int))

-- | Runs a @Machine@ with the number of regesters.
runMachine :: Machine a -> Int -> (a, [Asm.Inst])
runMachine m = runReader (evalStateT (runWriterT m) (Map.empty, Map.empty))

gen :: Address -> [ThreeAddress] -> Machine Asm.Operand
gen (Const c) _ = return $ genConst c
gen (Name s) xs = do
  mapM_ gen' xs
  m <- gets snd
  return $ maybe (error $ "gen: not found: " ++ show s) (locToOperand . head) $ Map.lookup s m
gen _ _ = undefined

genConst :: Constant -> Asm.Operand
genConst (CInt c) = Asm.Const . Asm.CInt64 $ fromIntegral c
genConst (CBool b) = Asm.Const . Asm.CInt8 . fromIntegral $ fromEnum b

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

type RegisterDescriptor = Map.Map Asm.Register [String]

type AddressDescriptor = Map.Map String [Location]

data Location =
    Reg Asm.Register
  | Mem Asm.Memory

locToOperand :: Location -> Asm.Operand
locToOperand (Reg r) = Asm.Reg r
locToOperand (Mem r) = Asm.Mem r
