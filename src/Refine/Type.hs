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

instance Types t => Types [t] where
  apply s = map $ apply s
  ftv = Set.unions . map ftv

(@@) :: Subst -> Subst -> Subst
a @@ b = Map.map (apply a) b `Map.union` a

merge :: Monad m => Subst -> Subst -> m Subst
merge a b = if False `elem` Map.elems (Map.intersectionWith (==) a b)
              then fail "merge fails"
              else return $ Map.union a b

mgu :: Monad m => Type1 -> Type1 -> m Subst
mgu (TypeApp a b) (TypeApp c d) = do
  s <- mgu a c
  t <- mgu (apply s b) $ apply s d
  return $ t @@ s

mgu (TypeVar1 u) t = varBind u t
mgu t (TypeVar1 u) = varBind u t
mgu (TypeCon c) (TypeCon c') | c == c' = return Map.empty
mgu _ _ = fail "types do not unify"

varBind :: Monad m => TVar -> Type1 -> m Subst
varBind u t
  | t == TypeVar1 u = return Map.empty
  | u `Set.member` ftv t = fail "occur check fails"
  | kind u /= kind t = fail "kinds do not match"
  | otherwise = return $ Map.singleton u t

match :: Monad m => Type1 -> Type1 -> m Subst
match (TypeApp a b) (TypeApp c d) = do
  s <- match a c
  t <- match b d
  merge s t

match (TypeVar1 u) t | kind u == kind t = varBind u t
match (TypeCon c) (TypeCon c') | c == c' = return Map.empty
match _ _ = fail "types do not match"

data Qual t = [Pred] :=> t
  deriving Eq

data Pred = IsIn String Type1
  deriving Eq

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  ftv (ps :=> t) = ftv ps `Set.union` ftv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i $ apply s t
  ftv (IsIn _ t) = ftv t

mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu
matchPred = lift match

lift :: Monad m => (Type1 -> Type1 -> m a) -> Pred -> Pred -> m a
lift m (IsIn i t) (IsIn j u)
  | i == j = m t u
  | otherwise = fail "classes differ"

type Class = ([String], [Inst])
type Inst = Qual Pred

data ClassEnv = ClassEnv
  { classes :: String -> Maybe Class
  , defaults :: [Type1]
  }

super :: ClassEnv -> String -> [String]
super ce i = case classes ce i of
  Just (is, its) -> is

insts :: ClassEnv -> String -> [Inst]
insts ce i = case classes ce i of
  Just (is, its) -> its

modifyEnv :: ClassEnv -> String -> Class -> ClassEnv
modifyEnv ce i c = ce
  { classes = \ j ->
      if i == j
        then Just c
        else classes ce j
  }

initialEnv :: ClassEnv
initialEnv = ClassEnv
  { classes = const $ fail "class not defined"
  , defaults = [tInt]
  }
