module Refine.Type where

import Control.Monad
import Control.Monad.State.Lazy hiding (lift)
import Data.Bifunctor
import Data.List (partition, (\\))
import qualified Data.Map.Lazy as Map
import Data.Map.Merge.Lazy hiding (merge)
import Data.Maybe
import qualified Data.Set as Set

import Refine.AST
import Refine.Kind

data Type =
    TypeVar TVar
  | TypeApp Type Type
  | TypeCon TCon
  | TypeGen Int
  deriving (Eq, Show)

data TVar = TVar String Kind
  deriving (Eq, Ord, Show)

data TCon = TCon String Kind
  deriving (Eq, Show)

tBool, tChar, tInt, tString, tList, tArrow, tTuple2 :: Type

tBool = TypeCon $ TCon "Bool" Star
tChar = TypeCon $ TCon "Char" Star
tInt = TypeCon $ TCon "Int" Star

tString = list tChar

tList = TypeCon . TCon "[]" $ KFun Star Star
tArrow = TypeCon . TCon "(->)" . KFun Star $ KFun Star Star
tTuple2 = TypeCon . TCon "(,)" . KFun Star $ KFun Star Star

fn :: Type -> Type -> Type
fn a = TypeApp $ TypeApp tArrow a

list :: Type -> Type
list = TypeApp tList

pair :: Type -> Type -> Type
pair = TypeApp . TypeApp tTuple2

class HasKind t where
  kind :: t -> Kind

instance HasKind TVar where
  kind (TVar _ k) = k

instance HasKind TCon where
  kind (TCon _ k) = k

instance HasKind Type where
  kind (TypeVar v) = kind v
  kind (TypeCon c) = kind c
  kind (TypeApp v _) = let (KFun _ k) = kind v in k

type Subst = Map.Map TVar Type

class Types t where
  apply :: Subst -> t -> t
  ftv :: t -> Set.Set TVar

instance Types Type where
  apply s v @ (TypeVar name) = Map.findWithDefault v name s
  apply s (TypeApp a b) = TypeApp (apply s a) (apply s b)
  apply _ t = t

  ftv (TypeVar v) = Set.singleton v
  ftv (TypeApp t u) = ftv t `Set.union` ftv u
  ftv _ = Set.empty

instance Types t => Types [t] where
  apply = map . apply
  ftv = Set.unions . map ftv

-- |
-- Composes two @Subst@ from right in series, i.e.
-- (@apply (a \@\@ b) = apply a . apply b@).
(@@) :: Subst -> Subst -> Subst
a @@ b = Map.map (apply a) b `Map.union` a

-- |
-- Composes two @Subst@ parallel. @merge@ is symmetric.
-- It fails if substitutions conflict.
merge :: Monad m => Subst -> Subst -> m Subst
merge = mergeA
          preserveMissing
          preserveMissing $
          zipWithAMatched . const $
            \a b -> if a == b
                      then pure a
                      else fail "merge fails"

-- |
-- Obtains the most general unifier on two types.
mgu :: Monad m => Type -> Type -> m Subst
mgu (TypeApp a b) (TypeApp c d) = do
  s <- mgu a c
  t <- mgu (apply s b) $ apply s d
  return $ t @@ s

mgu (TypeVar u) t = varBind u t
mgu t (TypeVar u) = varBind u t
mgu (TypeCon c) (TypeCon c') | c == c' = return Map.empty
mgu _ _ = fail "types do not unify"

varBind :: Monad m => TVar -> Type -> m Subst
varBind u t
  | t == TypeVar u = return Map.empty
  | u `Set.member` ftv t = fail "occur check fails"
  | kind u /= kind t = fail "kinds do not match"
  | otherwise = return $ Map.singleton u t

-- |
-- Matching is like unification, but it finds a substitution @s@ where
-- @apply s t1 = t2@.
match :: Monad m => Type -> Type -> m Subst
match (TypeApp a b) (TypeApp c d) = do
  s <- match a c
  t <- match b d
  merge s t

match (TypeVar u) t | kind u == kind t = varBind u t
match (TypeCon c) (TypeCon c') | c == c' = return Map.empty
match _ _ = fail "types do not match"

data Qual t = [Pred] :=> t
  deriving (Eq, Show)

data Pred = IsIn String Type
  deriving (Eq, Show)

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  ftv (ps :=> t) = ftv ps `Set.union` ftv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i $ apply s t
  ftv (IsIn _ t) = ftv t

-- | 'mgu' on 'Pred'.
mguPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu

-- | 'match' on 'Pred'.
matchPred :: Pred -> Pred -> Maybe Subst
matchPred = lift match

-- |
-- Lifts a function on two 'Type's to one on two 'Pred's and checks that
-- classes of 'Pred's are the same.
lift :: Monad m => (Type -> Type -> m a) -> Pred -> Pred -> m a
lift m (IsIn i t) (IsIn j u)
  | i == j = m t u
  | otherwise = fail "classes differ"

type Class = ([String], [Inst])
type Inst = Qual Pred

data ClassEnv = ClassEnv
  { classes :: Map.Map String Class
  , defaults :: [Type]
  }

-- | Gets superclasses of the given class name on an environment.
super :: ClassEnv -> String -> [String]
super ce i = case Map.lookup i $ classes ce of
  Just (is, _) -> is

-- | Gets instances of the given class name on an environment.
insts :: ClassEnv -> String -> [Inst]
insts ce i = case Map.lookup i $ classes ce of
  Just (_, its) -> its

-- |
-- Updates an environment, binding a class name to a 'Class'.
-- If the name is already defined, it will be overwritten.
modifyEnv :: ClassEnv -> String -> Class -> ClassEnv
modifyEnv ce i c = ce { classes = Map.insert i c $ classes ce }

-- | An initial class environment.
initialEnv :: ClassEnv
initialEnv = ClassEnv
  { classes = Map.empty
  , defaults = [tInt]
  }

type EnvTransformer = ClassEnv -> Maybe ClassEnv

-- | The same as '>=>' of monads, specialized to 'EnvTransformer'.
infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
f <:> g = f >=> g

-- |
-- Adds a class and its superclasses to the given environment.
-- It checks that the class is not defined yet and its superclasses are all defined.
addClass :: String -> [String] -> EnvTransformer
addClass i is ce
  | i `Map.member` classes ce = fail "class already defined"
  | any (`Map.notMember` classes ce) is = fail "superclass not defined"
  | otherwise = return $ modifyEnv ce i (is, [])

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p @ (IsIn i _) ce
  | i `Map.notMember` classes ce = fail "no class for instance"
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
byInst ce p @ (IsIn i _) = msum [tryInst it | it <- insts ce i]
  where
    tryInst :: Inst -> Maybe [Pred]
    tryInst (ps :=> h) = flip map ps . apply <$> matchPred h p

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = elem p `any` map (bySuper ce) ps || maybe False (all $ entail ce ps) (byInst ce p)

inHnf :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
  where
    hnf :: Type -> Bool
    hnf (TypeVar _) = True
    hnf (TypeCon _) = False
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

data Scheme = Forall [Kind] (Qual Type)
  deriving Eq

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks $ apply s qt
  ftv (Forall _ qt) = ftv qt

quantify :: [TVar] -> Qual Type -> Scheme
quantify vs qt = Forall ks $ apply s qt
  where
    vs' :: [TVar]
    vs' = Set.toList . Set.filter (`elem` vs) $ ftv qt

    ks :: [Kind]
    ks = map kind vs'

    s :: Subst
    s = Map.fromList . zip vs' $ map TypeGen [0..]

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

data Assump = String :>: Scheme

instance Types Assump where
  apply s (i :>:sc) = i :>: apply s sc
  ftv (_ :>: sc) = ftv sc

find :: Monad m => String -> [Assump] -> m Scheme
find i [] = fail $ "unbound identifier: " ++ i
find i ((i' :>: sc) : as)
  | i == i'   = return sc
  | otherwise =  find i as

type TI = State (Subst, Int)

runTI :: TI a -> a
runTI = flip evalState (Map.empty, 0)

getSubst :: TI Subst
getSubst = gets fst

unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) $ apply s t2
  extSubst u

extSubst :: Subst -> TI ()
extSubst = modify . first . (@@)

enumId :: Int -> String
enumId n = "v" ++ show n

newTVar :: Kind -> TI Type
newTVar k = state $ \(s, n) -> (TypeVar $ TVar (enumId n) k, (s, n + 1))

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
  ts <- mapM newTVar ks
  return $ inst ts qt

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TypeApp a b) = TypeApp (inst ts a) $ inst ts b
  inst ts (TypeGen n) = ts !! n
  inst _ t = t

instance Instantiate a => Instantiate [a] where
  inst ts = map $ inst ts

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c $ inst ts t

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

tiLit :: Literal -> TI ([Pred], Type)
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

tiPat :: Pat -> TI ([Pred], [Assump], Type)
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

tiPat (PCon (_ :>: sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t' <- newTVar Star
  (qs :=> t) <- freshInst sc
  unify t (foldr fn t' ts)
  return (ps ++ qs, as, t')

tiPats :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do
  triples <- mapM tiPat pats
  let ps = concatMap fst triples
      as = concatMap snd triples
      ts = map trd triples
  return (ps, as, ts)
  where
    fst (a, _, _) = a
    snd (_, b, _) = b
    trd (_, _, c) = c

tiExpr :: Infer Expr Type
tiExpr _ as (Var i) = do
  (ps :=> t) <- find i as >>= freshInst
  return (ps, t)
tiExpr _ _ (Lit l) = tiLit l
tiExpr ce as (App e f) = do
  (ps, te) <- tiExpr ce as e
  (qs, tf) <- tiExpr ce as f
  t <- newTVar Star
  unify (tf `fn` t) te
  return (ps ++ qs, t)

type Alt = ([Pat], Expr)

tiAlt :: Infer Alt Type
tiAlt ce as (pats, e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExpr ce (as' ++ as) e
  return (ps ++ qs, foldr fn t ts)

tiAlts :: ClassEnv -- ^ Class environment.
       -> [Assump] -- ^ Assumptions.
       -> [Alt]    -- ^ Alternatives.
       -> Type    -- ^ Expected type for the alternatives.
       -> TI [Pred]
tiAlts ce as alts t = do
  double <- forM alts $ tiAlt ce as
  mapM_ (unify t . snd) double
  return . concat $ map fst double

split :: Monad m => ClassEnv -> Set.Set TVar -> Set.Set TVar -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps = do
  ps' <- reduce ce ps
  let (ds, rs) = partition (all (`Set.member` fs) . ftv) ps'
  rs' <- defaultedPreds ce (Set.union fs gs) rs
  return (ds, rs \\ rs')

type Ambiguity = (TVar, [Pred])

ambiguities :: ClassEnv -> Set.Set TVar -> [Pred] -> [Ambiguity]
ambiguities _ vs ps = [(v, filter (elem v . ftv) ps) | v <- Set.toList $ ftv ps Set.\\ vs]

numClasses :: [String]
numClasses = ["Num"]

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) = [t' | all (TypeVar v ==) ts,
                              any (`elem` numClasses) is,
                              t' <- defaults ce,
                              all (entail ce []) [IsIn i t' | i <- is]]
  where
    is :: [String]
    is = [i | IsIn i _ <- qs]

    ts :: [Type]
    ts = [t | IsIn _ t <- qs]

withDefaults :: Monad m => (([Ambiguity], [Type]) -> a) -> ClassEnv -> Set.Set TVar -> [Pred] -> m a
withDefaults f ce vs ps
  | any null tss = fail "cannot resolve ambiguity"
  | otherwise = return $ f (vps, map head tss)
  where
    vps :: [Ambiguity]
    vps = ambiguities ce vs ps

    tss :: [[Type]]
    tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> Set.Set TVar -> [Pred] -> m [Pred]
defaultedPreds = withDefaults $ concatMap snd . fst

defaultSubst :: Monad m => ClassEnv -> Set.Set TVar -> [Pred] -> m Subst
defaultSubst = withDefaults $ Map.fromList . uncurry zip . first (map fst)

-- |
-- Explicitly typed binding.
-- It is a triple which consists of a name of the bound variable, a declared
-- type scheme and alternatives.
type Expl = (String, Scheme, [Alt])

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (_, sc, alts) = do
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
restricted = any simple
  where
    simple :: Impl -> Bool
    simple (_, alts) = any (null . fst) alts

tiImpls :: Infer [Impl] [Assump]
tiImpls ce as bs = do
  ts <- mapM (const $ newTVar Star) bs
  pss <- zipWithM (tiAlts ce $ getAssumps ts) (map snd bs) ts
  s <- getSubst
  (ds, rs) <- split ce (getFixed s) (foldr1 Set.intersection $ vssOf s ts) $ getPreds s pss
  return $ if restricted bs
             then (ds ++ rs, zipWith (:>:) is $ scs1 rs s ts)
             else (ds, zipWith (:>:) is $ scs2 rs s ts)
  where
    scs1 :: [Pred] -> Subst -> [Type] -> [Scheme]
    scs1 rs s ts = map (quantify (Set.toList $ getGenerics s ts Set.\\ ftv rs) . ([] :=>)) $ apply s ts

    scs2 :: [Pred] -> Subst -> [Type] -> [Scheme]
    scs2 rs s ts = map (quantify (Set.toList $ getGenerics s ts) . (rs :=>)) $ apply s ts

    is :: [String]
    is = map fst bs

    getAssumps :: [Type] -> [Assump]
    getAssumps ts = zipWith (:>:) is (map toScheme ts) ++ as

    getPreds :: Subst -> [[Pred]] -> [Pred]
    getPreds s = apply s . concat

    getFixed :: Subst -> Set.Set TVar
    getFixed s = ftv $ apply s as

    vssOf :: Subst -> [Type] -> [Set.Set TVar]
    vssOf s = map ftv . apply s

    getGenerics :: Subst -> [Type] -> Set.Set TVar
    getGenerics s ts = foldr1 Set.union (vssOf s ts) Set.\\ getFixed s

type BindGroup = ([Expl], [[Impl]])

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es, iss) = do
  (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
  qss <- mapM (tiExpl ce $ as'' ++ as' ++ as) es
  return (ps ++ concat qss, as'' ++ as')
  where
    as' :: [Assump]
    as' = [v :>: sc | (v, sc, _) <- es]

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq _ _ _ [] = return ([], [])
tiSeq ti ce as (bs : bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' ++ as) bss
  return (ps ++ qs, as'' ++ as')

type Program = [BindGroup]

tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram ce as bgs = runTI $ do
  (ps, as') <- tiSeq tiBindGroup ce as bgs
  s <- getSubst
  s' <- defaultSubst ce Set.empty =<< reduce ce (apply s ps)
  return $ apply (s' @@ s) as'
