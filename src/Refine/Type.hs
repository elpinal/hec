module Refine.Type where

import Refine.Kind

data Type =
    TypeInt
  | TypeBool
  | TypeChar
  | TypeString
  | TypeFun Type Type
  | TypeVar String
  deriving (Eq, Show)

data Type1 =
    TypeVar1 TVar
  | TypeApp Type1 Type1
  | TypeCon TCon

data TVar = TVar String Kind

data TCon = TCon String Kind

tBool, tChar, tInt, tList, tArrow, tTuple2 :: Type1

tBool = TypeCon $ TCon "Bool" Star
tChar = TypeCon $ TCon "Char" Star
tInt = TypeCon $ TCon "Int" Star

tList = TypeCon . TCon "[]" $ KFun Star Star
tArrow = TypeCon . TCon "(->)" . KFun Star $ KFun Star Star
tTuple2 = TypeCon . TCon "(,)" . KFun Star $ KFun Star Star
