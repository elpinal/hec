module Refine.Inter where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import Data.Maybe

import Refine.Parse

type Env a = ExceptT String (State InterState) a

evalEnv :: InterState -> Env a -> Either String a
evalEnv s = flip evalState s . runExceptT

isDefined :: String -> Env Bool
isDefined name = Map.member name <$> gets table

resolve :: String -> Env (Maybe Type)
resolve name = Map.lookup name <$> gets table

newTypeVar :: Env Type
newTypeVar = do
  n <- gets supply
  modify $ \s -> s {supply = n + 1}
  return . TypeVar $ "a" ++ show n

data InterState = InterState
  { table :: SymbolTable
  , supply :: Int
  }

interState :: InterState
interState = InterState
  { table = Map.empty
  , supply = 0
  }

type SymbolTable = Map.Map String Type

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
  t <- resolve name
  let opType = fromMaybe (error $ "not defined: " ++ show name) t
  l <- typeOf lhs
  r <- typeOf rhs
  case opType of
    TypeFun a (TypeFun b c)
      | a == l && b == r -> return c
    _ -> error "type mismatch"
typeOf (App f x) = TypeFun <$> typeOf f <*> typeOf x
typeOf (Var name) = do
  t <- resolve name
  return $ fromMaybe (error $ "not defined: " ++ show name) t
typeOf (Abs name body) = do
  tv <- newTypeVar
  s <- get
  put $ s { table = Map.insert name tv $ table s }
  t <- typeOf body
  modify $ \ss -> ss { table = table s }
  return t

litType :: Literal -> Type
litType (LitInt _) = TypeInt
litType (LitBool _) = TypeBool
litType (LitChar _) = TypeChar
litType (LitString _) = TypeString
