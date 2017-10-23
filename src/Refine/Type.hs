module Refine.Type where

import Control.Monad
import Control.Monad.State.Lazy hiding (lift)
import Data.Bifunctor
import Data.List (partition, (\\))
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set

import Refine.AST
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
  | TypeGen Int
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

type EnvTransformer = ClassEnv -> Maybe ClassEnv

infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
f <:> g = f >=> g

addClass :: String -> [String] -> EnvTransformer
addClass i is ce
  | isJust (classes ce i) = fail "class already defined"
  | any (isNothing . classes ce) is = fail "superclass not defined"
  | otherwise = return $ modifyEnv ce i (is, [])

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p @ (IsIn i _) ce
  | classes ce i == Nothing = fail "no class for instance"
  | any (overlap p) qs = fail "overlapping instance"
  | otherwise = return $ modifyEnv ce i c
  where
    its :: [Inst]
    its = insts ce i

    qs :: [Pred]
    qs = [q | (_ :=> q) <- its]

    c :: ([String], [Inst])
    c = (super ce i, (ps :=> p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = isJust $ mguPred p q

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p @ (IsIn i t) = p : concat [bySuper ce $ IsIn i' t | i' <- super ce i]

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p @ (IsIn i t) = msum [tryInst it | it <- insts ce i]
  where
    tryInst :: Inst -> Maybe [Pred]
    tryInst (ps :=> h) = flip map ps . apply <$> matchPred h p

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = elem p `any` map (bySuper ce) ps || maybe False (all $ entail ce ps) (byInst ce p)

inHnf :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
  where
    hnf :: Type1 -> Bool
    hnf (TypeVar1 v) = True
    hnf (TypeCon tc) = False
    hnf (TypeApp t _) = hnf t

toHnfs :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = concat <$> mapM (toHnf ce) ps

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p
  | inHnf p = return [p]
  | otherwise = maybe (fail "context reduction") (toHnfs ce) $ byInst ce p

simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where
    loop :: [Pred] -> [Pred] -> [Pred]
    loop rs [] = rs
    loop rs (p : ps)
      | entail ce (rs ++ ps) p = loop rs ps
      | otherwise = loop (p : rs) ps

reduce :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = simplify ce <$> toHnfs ce ps

data Scheme = Forall [Kind] (Qual Type1)
  deriving Eq

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks $ apply s qt
  ftv (Forall _ qt) = ftv qt

quantify :: [TVar] -> Qual Type1 -> Scheme
quantify vs qt = Forall ks $ apply s qt
  where
    vs' :: [TVar]
    vs' = Set.toList . Set.filter (`elem` vs) $ ftv qt

    ks :: [Kind]
    ks = map kind vs'

    s :: Subst
    s = Map.fromList . zip vs' $ map TypeGen [0..]

toScheme :: Type1 -> Scheme
toScheme t = Forall [] ([] :=> t)

data Assump = String :>: Scheme

instance Types Assump where
  apply s (i :>:sc) = i :>: apply s sc
  ftv (i :>: sc) = ftv sc

find :: Monad m => String -> [Assump] -> m Scheme
find i [] = fail $ "unbound identifier: " ++ i
find i ((i' :>: sc) : as) =
  if i == i'
    then return sc
    else find i as

type TI = State (Subst, Int)

runTI :: TI a -> a
runTI = flip evalState (Map.empty, 0)

getSubst :: TI Subst
getSubst = gets fst

unify :: Type1 -> Type1 -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) $ apply s t2
  extSubst u

extSubst :: Subst -> TI ()
extSubst = modify . first . (@@)

enumId :: Int -> String
enumId n = "v" ++ show n

newTVar :: Kind -> TI Type1
newTVar k = state $ \(s, n) -> (TypeVar1 $ TVar (enumId n) k, (s, n + 1))

freshInst :: Scheme -> TI (Qual Type1)
freshInst (Forall ks qt) = do
  ts <- mapM newTVar ks
  return $ inst ts qt

class Instantiate t where
  inst :: [Type1] -> t -> t

instance Instantiate Type1 where
  inst ts (TypeApp a b) = TypeApp (inst ts a) $ inst ts b
  inst ts (TypeGen n) = ts !! n
  inst ts t = t

instance Instantiate a => Instantiate [a] where
  inst ts = map $ inst ts

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c $ inst ts t

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

tiLit :: Literal -> TI ([Pred], Type1)
tiLit (LitChar _) = return ([], tChar)
tiLit (LitInt _) = do
  v <- newTVar Star
  return ([IsIn "Num" v], v)
tiLit (LitString _) = return ([], tString)

data Pat =
    PVar String
  | PWildcard
  | PAs String Pat
  | PLit Literal
  | PCon Assump [Pat]

tiPat :: Pat -> TI ([Pred], [Assump], Type1)
tiPat (PVar i) = do
  v <- newTVar Star
  return ([], [i :>: toScheme v], v)

tiPat PWildcard = do
  v <- newTVar Star
  return ([], [], v)

tiPat (PAs i pat) = do
  (ps, as, t) <- tiPat pat
  return (ps, (i :>: toScheme t) : as, t)

tiPat (PLit l) = do
  (ps, t) <- tiLit l
  return (ps, [], t)

tiPat (PCon (i :>: sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t' <- newTVar Star
  (qs :=> t) <- freshInst sc
  unify t (foldr fn t' ts)
  return (ps ++ qs, as, t')

tiPats :: [Pat] -> TI ([Pred], [Assump], [Type1])
tiPats pats = do
  triples <- mapM tiPat pats
  let ps = concat . map fst $ triples
      as = concat . map snd $ triples
      ts = map trd triples
  return (ps, as, ts)
  where
    fst (a, _, _) = a
    snd (_, b, _) = b
    trd (_, _, c) = c

tiExpr :: Infer Expr Type1
tiExpr ce as (Var i) = do
  (ps :=> t) <- find i as >>= freshInst
  return (ps, t)
tiExpr ce as (Lit l) = tiLit l
tiExpr ce as (App e f) = do
  (ps, te) <- tiExpr ce as e
  (qs, tf) <- tiExpr ce as f
  t <- newTVar Star
  unify (tf `fn` t) te
  return (ps ++ qs, t)

type Alt = ([Pat], Expr)

tiAlt :: Infer Alt Type1
tiAlt ce as (pats, e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExpr ce (as' ++ as) e
  return (ps ++ qs, foldr fn t ts)

tiAlts :: ClassEnv -- ^ Class environment.
       -> [Assump] -- ^ Assumptions.
       -> [Alt]    -- ^ Alternatives.
       -> Type1    -- ^ Expected type for the alternatives.
       -> TI [Pred]
tiAlts ce as alts t = do
  double <- mapM (tiAlt ce as) alts
  mapM (unify t) $ map snd double
  return . concat $ map fst double

split :: Monad m => ClassEnv -> Set.Set TVar -> Set.Set TVar -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps = do
  ps' <- reduce ce ps
  let (ds, rs) = partition (all (`Set.member` fs) . ftv) ps'
  rs' <- defaultedPreds ce (Set.union fs gs) rs
  return (ds, rs \\ rs')

type Ambiguity = (TVar, [Pred])

ambiguities :: ClassEnv -> Set.Set TVar -> [Pred] -> [Ambiguity]
ambiguities ce vs ps = [(v, filter (elem v . ftv) ps) | v <- Set.toList $ ftv ps Set.\\ vs]

numClasses :: [String]
numClasses = ["Num"]

candidates :: ClassEnv -> Ambiguity -> [Type1]
candidates ce (v, qs) = [t' | all (TypeVar1 v ==) ts,
                              any (`elem` numClasses) is,
                              t' <- defaults ce,
                              all (entail ce []) [IsIn i t' | i <- is]]
  where
    is :: [String]
    is = [i | IsIn i _ <- qs]

    ts :: [Type1]
    ts = [t | IsIn _ t <- qs]

withDefaults :: Monad m => (([Ambiguity], [Type1]) -> a) -> ClassEnv -> Set.Set TVar -> [Pred] -> m a
withDefaults f ce vs ps
  | any null tss = fail "cannot resolve ambiguity"
  | otherwise = return $ f (vps, map head tss)
  where
    vps :: [Ambiguity]
    vps = ambiguities ce vs ps

    tss :: [[Type1]]
    tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> Set.Set TVar -> [Pred] -> m [Pred]
defaultedPreds = withDefaults $ concat . map snd . fst

defaultSubst :: Monad m => ClassEnv -> Set.Set TVar -> [Pred] -> m Subst
defaultSubst = withDefaults $ Map.fromList . uncurry zip . first (map fst)

-- |
-- Explicitly typed binding.
-- It is a triple which consists of a name of the bound variable, a declared
-- type scheme and alternatives.
type Expl = (String, Scheme, [Alt])

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (i, sc, alts) = do
  (qs :=> t) <- freshInst sc
  ps <- tiAlts ce as alts t
  s <- getSubst
  let
    qs' = apply s qs
    t' = apply s t
    fs = ftv $ apply s as
    gs = ftv t' Set.\\ fs
    sc' = quantify (Set.toList gs) $ qs' :=> t'
    ps' = filter (not . entail ce qs') $ apply s ps
  (ds, rs) <- split ce fs gs ps'
  when (sc /= sc') $
       fail "signature too general"
  when (rs /= []) $
       fail "context too weak"
  return ds

type Impl = (String, [Alt])

restricted :: [Impl] -> Bool
restricted bs = any simple bs
  where
    simple :: Impl -> Bool
    simple (i, alts) = any (null . fst) alts

tiImpls :: Infer [Impl] [Assump]
tiImpls ce as bs = do
  ts <- mapM (\_ -> newTVar Star) bs
  let is = map fst bs
      scs = map toScheme ts
      as' = zipWith (:>:) is scs ++ as
      altss = map snd bs
  pss <- sequence (zipWith (tiAlts ce as') altss ts)
  s <- getSubst
  let ps' = apply s $ concat pss
      ts' = apply s ts
      fs = ftv $ apply s as
      vss = map ftv ts'
      gs = foldr1 Set.union vss Set.\\ fs
  (ds, rs) <- split ce fs (foldr1 Set.intersection vss) ps'
  if restricted bs then
    let gs' = Set.toList $ gs Set.\\ ftv rs
        scs' = map (quantify gs' . ([] :=>)) ts'
    in return (ds ++ rs, zipWith (:>:) is scs')
  else
    let scs' = map (quantify (Set.toList gs) . (rs :=>)) ts'
    in return (ds, zipWith (:>:) is scs')

type BindGroup = ([Expl], [[Impl]])

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es, iss) = do
  (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
  qss <- mapM (tiExpl ce $ as'' ++ as' ++ as) es
  return (ps ++ concat qss, as'' ++ as')
  where
    as' :: [Assump]
    as' = [v :>: sc | (v, sc, alts) <- es]

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as [] = return ([], [])
tiSeq ti ce as (bs : bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' ++ as) bss
  return (ps ++ qs, as'' ++ as')

type Program = [BindGroup]

tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram ce as bgs = runTI $ do
  (ps, as') <- tiSeq tiBindGroup ce as bgs
  s <- getSubst
  s' <- defaultSubst ce Set.empty =<< (reduce ce $ apply s ps)
  return $ apply (s' @@ s) as'
