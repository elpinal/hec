module Refine.Type where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

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
  deriving (Eq, Show)

data TVar = TVar String Kind
  deriving (Eq, Ord, Show)

data TCon = TCon String Kind
  deriving (Eq, Show)

tBool, tChar, tInt, tString, tList, tArrow, tTuple2 :: Type1

tBool = TypeCon $ TCon "Bool" Star
tChar = TypeCon $ TCon "Char" Star
tInt = TypeCon $ TCon "Int" Star

tString = list tChar

tList = TypeCon . TCon "[]" $ KFun Star Star
tArrow = TypeCon . TCon "(->)" . KFun Star $ KFun Star Star
tTuple2 = TypeCon . TCon "(,)" . KFun Star $ KFun Star Star

fn :: Type1 -> Type1 -> Type1
fn a = TypeApp $ TypeApp tArrow a

list :: Type1 -> Type1
list = TypeApp tList

pair :: Type1 -> Type1 -> Type1
pair a = TypeApp $ TypeApp tTuple2 a

class HasKind t where
  kind :: t -> Kind

instance HasKind TVar where
  kind (TVar _ k) = k

instance HasKind TCon where
  kind (TCon _ k) = k

instance HasKind Type1 where
  kind (TypeVar1 v) = kind v
  kind (TypeApp v u) = case kind v of
    (KFun k _) -> k
  kind (TypeCon c) = kind c

type Subst = Map.Map TVar Type1

class Types t where
  apply :: Subst -> t -> t
  ftv :: t -> Set.Set TVar

instance Types Type1 where
  apply s v @ (TypeVar1 name) = Map.findWithDefault v name s
  apply s (TypeApp a b) = TypeApp (apply s a) (apply s b)
  apply _ t = t

  ftv (TypeVar1 v) = Set.singleton v
  ftv (TypeApp t u) = ftv t `Set.union` ftv u
  ftv _ = Set.empty

(@@) :: Subst -> Subst -> Subst
a @@ b = Map.map (apply a) b `Map.union` a

merge :: Monad m => Subst -> Subst -> m Subst
merge a b = if False `elem` Map.elems (Map.intersectionWith (==) a b)
              then fail "merge fails"
              else return $ Map.union a b
