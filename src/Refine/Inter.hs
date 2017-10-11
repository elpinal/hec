module Refine.Inter where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

import Refine.Parse

type Env a = ExceptT String (State SymbolTable) a

isDefined :: String -> Env Bool
isDefined name = Map.member name <$> get

resolve :: String -> Env (Maybe Expr)
resolve name = Map.lookup name <$> get

type SymbolTable = Map.Map String Expr

data Type =
    TypeInt
  | TypeBool
  | TypeChar
  | TypeString
  | TypeFun Type Type
  -- | Var String
  deriving (Eq, Show)

typeOf :: Expr -> Env Type
typeOf (Lit lit) = return $ litType lit
typeOf (BinOp name lhs rhs) = do
  e <- resolve name
  opType <- maybe (error $ "not defined: " ++ show name) typeOf e
  l <- typeOf lhs
  r <- typeOf rhs
  case opType of
    TypeFun a (TypeFun b c)
      | a == l && b == r -> return c
    _ -> error "type mismatch"
typeOf (App f x) = TypeFun <$> typeOf f <*> typeOf x
typeOf (Var name) = do
  e <- resolve name
  maybe (error $ "not defined: " ++ show name) typeOf e

litType :: Literal -> Type
litType (LitInt _) = TypeInt
litType (LitBool _) = TypeBool
litType (LitChar _) = TypeChar
litType (LitString _) = TypeString
