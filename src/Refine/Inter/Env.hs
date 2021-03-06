module Refine.Inter.Env
  ( emptyDecls
  , scanDecls
  , updateVars
  , updateTypes
  , runKindEnv
  , kindOf

  , DeclError(..)
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

import qualified Refine.AST as AST
import Refine.Kind
import qualified Refine.Type.Syntactic as S

type Vars  = Map.Map String (Maybe AST.Expr, Maybe S.Type)
type Types = Map.Map String S.Type

data Decls = Decls
  { vars :: Vars
  , types :: Types
  }
  deriving (Eq, Show)

emptyDecls :: Decls
emptyDecls = Decls
  { vars = Map.empty
  , types = Map.empty
  }

updateVars :: (Vars -> Vars) -> Decls -> Decls
updateVars f d = d { vars = f $ vars d }

updateTypes :: (Types -> Types) -> Decls -> Decls
updateTypes f d = d { types = f $ types d }

data DeclError =
    Duplicate String
  | Undefined String
  | AppOfStar
  | KindMismatch
  deriving (Eq, Show)

scanDecls :: [AST.Decl] -> Either DeclError Decls
scanDecls ds = foldl (>=>) return (map scanDecl ds) emptyDecls
  where
    scanDecl :: AST.Decl -> Decls -> Either DeclError Decls

    scanDecl (AST.VarDecl i e) d =
      case Map.lookup i $ vars d of
        Just (Just e, t)  -> Left $ Duplicate i
        Just (Nothing, t) -> defineVar i (Just e) t d
        Nothing           -> defineVar i (Just e) Nothing d

    scanDecl (AST.TypeAnn i t) d =
      case Map.lookup i $ vars d of
        Just (e, Just t)  -> Left $ Duplicate i
        Just (e, Nothing) -> defineVar i e (Just t) d
        Nothing           -> defineVar i Nothing (Just t) d

    scanDecl (AST.TypeDecl i t) d =
      if i `Map.member` types d
        then Left $ Duplicate i
        else defineType i t d

    scanDecl (AST.DataDecl i t) d =
      if i `Map.member` types d
        then Left $ Duplicate i
        else defineType i t d

defineVar :: Monad m => String -> Maybe AST.Expr -> Maybe S.Type -> Decls -> m Decls
defineVar i e t = return . updateVars (Map.insert i (e, t))

defineType :: Monad m => String -> S.Type -> Decls -> m Decls
defineType i t = return . updateTypes (Map.insert i t)

type KindEnv = ExceptT DeclError (State (Map.Map String Kind))

runKindEnv :: KindEnv a -> Map.Map String Kind -> Either DeclError a
runKindEnv e = evalState $ runExceptT e

getKind :: String -> KindEnv (Maybe Kind)
getKind i = Map.lookup i <$> get

kindOf :: S.Type -> Types -> KindEnv Kind
kindOf (S.TypeCon i) ts = maybe (primKind i) (\t -> kindOf t ts) $ Map.lookup i ts
kindOf (S.TypeApp t1 t2) ts = do
  k1 <- kindOf t1 ts
  k2 <- kindOf t2 ts
  case k1 of
    Star -> throwError AppOfStar
    KFun k k' | k1 == k -> return k'
    _ -> throwError KindMismatch

primKind :: String -> KindEnv Kind
primKind i = getKind i >>= maybe (throwError $ Undefined i) return
