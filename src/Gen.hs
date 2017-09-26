module Gen where

import Control.Arrow
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
    str (Code Mov xs) = "mov " ++ foldl1 joinWithComma (map strArg xs)
    str (Code op xs) = map toLower (show op) ++ " " ++ foldl1 joinWithComma (map strArg xs)

    joinWithComma :: String -> String -> String
    joinWithComma s t = s ++ ", " ++ t

    strArg :: Arg -> String
    strArg (Reg reg) = '%' : map toLower (show reg)
    strArg (Const v) = '$' : show v

generate :: Block -> (Seq Code, Register)
generate block = uncurry (gen . Set.fromList $ [EAX, EBX, ECX, EDX]) . second viewr $ checkLabel block

noAddr :: Show a => a -> b
noAddr x = error $ "unexpected error: no such address: " ++ show x

withDefault :: ViewR Inter.Quad -> a -> (ViewR Inter.Quad -> a) -> a
withDefault Sequence.EmptyR e _ = e
withDefault pre _ f = f pre

declOfAddrR :: Inter.Addr -> Seq Inter.Quad -> ViewR Inter.Quad
declOfAddrR a xs = viewr $ dropWhileR (maybe False (/= a) . resultAddr) xs
  where
    resultAddr :: Inter.Quad -> Maybe Inter.Addr
    resultAddr (Inter.Point addr, _, _, _) = Just addr
    resultAddr _ = Nothing

gen :: Set.Set Register -> Map.Map Inter.Addr Int -> ViewR Inter.Quad -> (Seq Code, Register)
gen registers m (xs:>(_, op, Inter.At addr1, Inter.At addr2)) =
  if getAddr addr1 > getAddr addr2 then
    g addr1 addr2
  else
    g addr2 addr1
  where
    getAddr :: Inter.Addr -> Int
    getAddr a = Map.findWithDefault (noAddr a) a m

    g :: Inter.Addr -> Inter.Addr -> (Seq Code, Register)
    g a b =
      withDefault (declOfAddrR a xs) (noAddr a) $
        withDefault (declOfAddrR b xs) (noAddr b) .
          uncurry f . gen registers m

    f :: Seq Code -> Register -> ViewR Inter.Quad -> (Seq Code, Register)
    f codes1 reg1 =
      flip (,) reg1 . uncurry (flip $ flip ((|>) . (codes1 ><)) . Code (instr op) . (: [Reg reg1]) . Reg) . gen (Set.delete reg1 registers) m

gen registers _ (_:>(_, op, Inter.Const v1, Inter.Const v2)) =
  (Sequence.fromList [Code Mov [Const v1, Reg reg], Code (instr op) [Const v2, Reg reg]], reg)
  where
    reg :: Register
    reg = Set.findMin registers

gen registers m (xs:>(_, op, Inter.At addr, Inter.Const v)) =
  withDefault
    (declOfAddrR addr xs)
    (noAddr addr) $
    (uncurry (flip $ flip (|>) . Code (instr op) . (Const v :) . return . Reg) &&& snd) . gen registers m

gen registers m (xs:>(_, op, Inter.Const v, Inter.At addr)) =
  withDefault
    (declOfAddrR addr xs)
    (noAddr addr) $
    flip (,) reg1 . uncurry (flip $ flip ((|>) . (|> Code Mov [Const v, Reg reg1])) . Code (instr op) . (: [Reg reg1]) . Reg) . gen registers m
  where
    reg1 :: Register
    reg1 = Set.findMin registers

gen registers _ (_:>(_, Inter.NOP, Inter.Const v, Inter.Nil)) =
  (singleton $ Code Mov [Const v, Reg reg], reg)
  where
    reg :: Register
    reg = Set.findMin registers

gen registers m (xs:>(_, Inter.NOP, Inter.At addr, Inter.Nil)) =
  withDefault
    (declOfAddrR addr xs)
    (noAddr addr) $
    gen registers m

gen registers m xs = error $
  "gen: unsupported arguments: " ++ foldl joinWithSpace [] [show registers, show m, show xs]
  where
    joinWithSpace :: String -> String -> String
    joinWithSpace s t = s ++ " " ++ t

checkLabel :: Block -> (Map.Map Inter.Addr Int, Seq Inter.Quad)
checkLabel = foldl checkLabel' (Map.empty, empty)

checkLabel' :: (Map.Map Inter.Addr Int, Seq Inter.Quad) -> Inter.Quad -> (Map.Map Inter.Addr Int, Seq Inter.Quad)
checkLabel' (m, xs) x@(Inter.Point addr,_,_,_) = (Map.insert addr l m, xs |> x)
  where
    l :: Int
    l = label x m

label :: Inter.Quad -> Map.Map Inter.Addr Int -> Int
label (_, Inter.NOP, _, Inter.Nil) _ = 1
label (_, _, operand1, Inter.Nil) m = labelOf operand1 m
label (_, _, operand1, operand2) m = 1 + min (labelOf operand1 m) (labelOf operand2 m)

labelOf :: Inter.Operand -> Map.Map Inter.Addr Int -> Int
labelOf (Inter.Const _) _ = 0
labelOf (Inter.At addr) m = Map.findWithDefault (error $ "no such address in the block: " ++ show addr) addr m
labelOf Inter.Nil _ = error "unexpected operand: nil"
