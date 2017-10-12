module Refine.Inter where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

import Refine.Parse

type Env a = ExceptT String (State InterState) a

evalEnv :: InterState -> Env a -> Either String a
evalEnv s = flip evalState s . runExceptT

isDefined :: String -> Env Bool
isDefined name = Map.member name <$> gets table

resolve :: String -> Env (Maybe (Type, Maybe Fixity))
resolve name = Map.lookup name <$> gets table

resolveE :: String -> Env (Type, Maybe Fixity)
resolveE name = resolve name >>= maybe (throwError $ "not defined: " ++ show name) return

newTypeVar :: Env Type
newTypeVar = do
  s <- get
  put s { supply = supply s + 1 }
  return . TypeVar $ "a" ++ show (supply s)

data InterState = InterState
  { table :: SymbolTable
  , supply :: Int
  }

interState :: InterState
interState = InterState
  { table = Map.empty
  , supply = 0
  }

type SymbolTable = Map.Map String (Type, Maybe Fixity)

data Fixity = Fixity (Maybe Direction) Precedence

data Direction =
    LeftAssoc
  | RightAssoc
  deriving Eq

type Precedence = Int

defaultFixity :: Fixity
defaultFixity = Fixity (Just LeftAssoc) 9

getFixity :: String -> Env Fixity
getFixity name = do
  info <- resolveE name
  flip (flip maybe return) (snd info) $ do
    s <- get
    put s { table = Map.insert name (fst info, Just defaultFixity) $ table s }
    return defaultFixity

recons :: Expr -> Env Expr
recons x @ (BinOp name (BinOp name1 lhs1 rhs1) rhs) = do
  (Fixity d p) <- getFixity name
  f <- getFixity name1
  case f of
    (Fixity _ p1)
      | p < p1 -> return x
      | p > p1 -> return . BinOp name1 lhs1 $ BinOp name rhs1 rhs
    (Fixity d1 @ (Just LeftAssoc) _)
      | d1 == d -> return x
    (Fixity d1 @ (Just RightAssoc) _)
      | d1 == d -> return . BinOp name1 lhs1 $ BinOp name rhs1 rhs
    (Fixity d1 @ Nothing _)
      | d1 == d -> throwError "cannot associate two non-associative operators"
    otherwise -> throwError $ "cannot mix " ++ show name1 ++ " and " ++ show name
recons x = return x

data Type =
    TypeInt
  | TypeBool
  | TypeChar
  | TypeString
  | TypeFun Type Type
  | TypeVar String
  deriving (Eq, Show)

typeOf :: Expr -> Env Type
typeOf (Lit lit) = return $ litType lit

typeOf (BinOp name lhs rhs) = do
  op <- resolveE name
  l <- typeOf lhs
  r <- typeOf rhs
  case fst op of
    TypeFun a (TypeFun b c)
      | a == l && b == r -> return c
    _ -> throwError "type mismatch"

typeOf (App f x) = do
  ft <- typeOf f
  xt <- typeOf x
  case ft of
    TypeFun a b | a == xt -> return b
    _ -> throwError $ "expected function type which takes " ++ show xt ++ ", but got " ++ show ft

typeOf (Var name) = fst <$> resolveE name

typeOf (Abs name body) = do
  tv <- newTypeVar
  s <- get
  put s { table = Map.insert name (tv, Nothing) $ table s }
  t <- typeOf body
  modify $ \ss -> ss { table = table s }
  return $ TypeFun tv t

litType :: Literal -> Type
litType (LitInt _) = TypeInt
litType (LitBool _) = TypeBool
litType (LitChar _) = TypeChar
litType (LitString _) = TypeString
