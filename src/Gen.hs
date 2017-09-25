module Gen where

import Control.Applicative (liftA2)
import Data.Char
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Sequence as Sequence
import qualified Data.Set as Set

import qualified Inter

type Block = [Inter.Quad]

data Instr = Add
           | Sub
           | Mov
           | IMul
           deriving (Show)

data Arg = Reg Register
         | Const Int
         deriving (Show)

data Register = EAX
              | EBX
              | ECX
              | EDX
              deriving (Show, Eq, Ord)

data Code = Code Instr [Arg] deriving (Show)

instr :: Inter.Op -> Instr
instr (Inter.Arith Inter.Add) = Add
instr (Inter.Arith Inter.Sub) = Sub
instr (Inter.Arith Inter.Mul) = IMul
instr Inter.NOP = error "not implemented yet"
instr op = error $ "no corresponding instruction: " ++ show op

codeToString :: Seq Code -> Seq String
codeToString = fmap str
  where
    str :: Code -> String
    str (Code Mov xs) = "mov " ++ foldl1 ((++) . (++ ", ")) (map strArg xs)
    str (Code op xs) = map toLower (show op) ++ " " ++ foldl1 ((++) . (++ ", ")) (map strArg xs)

    strArg :: Arg -> String
    strArg (Reg reg) = '%' : map toLower (show reg)
    strArg (Const v) = '$' : show v

generate :: Block -> (Seq Code, Register)
generate block = uncurry (gen . viewr) (checkLabel block) $ Set.fromList [EAX, EBX, ECX, EDX]

gen :: ViewR Inter.Quad -> Map.Map Inter.Addr Int -> Set.Set Register -> (Seq Code, Register)
gen (xs:>(_, op, Inter.At addr1, Inter.At addr2)) m registers =
  if fromMaybe (error "unexpected error") $ Map.lookup addr1 m .> Map.lookup addr2 m then
    case viewr $ dropWhileR (\(Inter.Point addr, _, _, _) -> addr /= addr1) xs of
    Sequence.EmptyR -> error $ "unexpected error: no such address: " ++ show addr1
    pre -> let
             (codes1, reg1) = gen pre m registers
           in
             case viewr $ dropWhileR (\(Inter.Point addr, _, _, _) -> addr /= addr2) xs of
             Sequence.EmptyR -> error $ "unexpected error: no such address: " ++ show addr2
             pre -> let
                      (codes2, reg2) = gen pre m $ Set.delete reg1 registers
                    in ((codes1 >< codes2) |> Code (instr op) [Reg reg2, Reg reg1], reg1)
  else
    case viewr $ dropWhileR (\(Inter.Point addr, _, _, _) -> addr /= addr2) xs of
    Sequence.EmptyR -> error $ "unexpected error: no such address: " ++ show addr1
    pre -> let
             (codes1, reg1) = gen pre m registers
           in
             case viewr $ dropWhileR (\(Inter.Point addr, _, _, _) -> addr /= addr1) xs of
               Sequence.EmptyR -> error $ "unexpected error: no such address: " ++ show addr2
               pre -> let
                        (codes2, reg2) = gen pre m $ Set.delete reg1 registers
                      in ((codes1 >< codes2) |> Code (instr op) [Reg reg2, Reg reg1], reg1)
  where (.>) = liftA2 (>)
gen (_:>(_, op, Inter.Const v1, Inter.Const v2)) _ registers =
  let reg = Set.findMin registers
    in (Sequence.fromList [Code Mov [Const v1, Reg reg], Code (instr op) [Const v2, Reg reg]], reg)
gen (xs:>(_, op, Inter.At addr, Inter.Const v)) m registers =
  case viewr $ dropWhileR (\(Inter.Point a, _, _, _) -> a /= addr) xs of
  Sequence.EmptyR -> error $ "unexpected error: no such address: " ++ show addr
  pre -> let (codes, reg) = gen pre m registers
    in (codes |> Code (instr op) [Const v, Reg reg], reg)
gen (xs:>(_, op, Inter.Const v, Inter.At addr)) m registers =
  case viewr $ dropWhileR (\(Inter.Point a, _, _, _) -> a /= addr) xs of
  Sequence.EmptyR -> error $ "unexpected error: no such address: " ++ show addr
  pre -> let
    (codes, reg) = gen pre m registers
    reg1 = Set.findMin registers
    in (codes |> Code Mov [Const v, Reg reg1] |> Code (instr op) [Reg reg, Reg reg1], reg1)
gen (_:>(_, Inter.NOP, Inter.Const v, Inter.Nil)) _ registers =
  let reg = Set.findMin registers
    in (singleton $ Code Mov [Const v, Reg reg], reg)
gen (xs:>(_, Inter.NOP, Inter.At addr, Inter.Nil)) m registers =
  case viewr $ dropWhileR (\(Inter.Point a, _, _, _) -> a /= addr) xs of
  Sequence.EmptyR -> error $ "unexpected error: no such address: " ++ show addr
  pre -> gen pre m registers
gen xs m registers = error $ "unexpected error: " ++ show xs ++ show m ++ show registers

checkLabel :: Block -> (Seq Inter.Quad, Map.Map Inter.Addr Int)
checkLabel block = foldl checkLabel' (empty, Map.empty) block

checkLabel' :: (Seq Inter.Quad, Map.Map Inter.Addr Int) -> Inter.Quad -> (Seq Inter.Quad, Map.Map Inter.Addr Int)
checkLabel' (xs, m) x@(Inter.Point addr,_,_,_) = (xs |> x, Map.insert addr l m)
  where l = label x m

label :: Inter.Quad -> Map.Map Inter.Addr Int -> Int
label (_, Inter.NOP, _, Inter.Nil) _ = 1
label (_, _, operand1, Inter.Nil) m = labelOf operand1 m
label (_, _, operand1, operand2) m = 1 + min (labelOf operand1 m) (labelOf operand2 m)

labelOf :: Inter.Operand -> Map.Map Inter.Addr Int -> Int
labelOf (Inter.Const _) _ = 0
labelOf (Inter.At addr) m = fromMaybe (error $ "no such address in the block: " ++ show addr) $ Map.lookup addr m
labelOf Inter.Nil _ = error "unexpected operand: nil"
