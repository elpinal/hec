module Refine.Type.Syntactic where

data Type =
    TypeVar String
  | TypeCon String
  | TypeApp Type Type
  deriving (Eq, Show)

tArrow :: Type
tArrow = TypeCon "(->)"

fn :: Type -> Type -> Type
fn a = TypeApp $ TypeApp tArrow a

tUnit :: Type
tUnit = TypeCon "()"

tTupleN :: Int -> Type
tTupleN n = TypeCon $ "(," ++ show n ++ ")"

tRecordN :: [String] -> Type
tRecordN xs = TypeCon $ "{" ++ concatWith "," xs ++ "}"

concatWith :: String -> [String] -> String
concatWith _ [] = ""
concatWith _ [x] = x
concatWith s (x : y : ys) = x ++ s ++ y ++ concatWith s ys
