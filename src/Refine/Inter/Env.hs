module Refine.Inter.Env
  ( emptyDecls
  , scanDecls
  , updateVars

  , DeclError(..)
  ) where

import Control.Monad
import qualified Data.Map.Lazy as Map

import qualified Refine.AST as AST
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
  deriving (Eq, Show)

scanDecls :: [AST.Decl] -> Either DeclError Decls
scanDecls ds = foldl (>=>) return (map scanDecl ds) emptyDecls
  where
    scanDecl :: AST.Decl -> Decls -> Either DeclError Decls

    scanDecl (AST.VarDecl i e) d =
      case Map.lookup i $ vars d of
        Just (Just e, t) -> Left $ Duplicate i
        Just (Nothing, t) -> return $ updateVars (Map.insert i (Just e, t)) d
        Nothing -> return $ updateVars (Map.insert i (Just e, Nothing)) d

    scanDecl (AST.TypeAnn i t) d =
      case Map.lookup i $ vars d of
        Just (e, Just t) -> Left $ Duplicate i
        Just (e, Nothing) -> return $ updateVars (Map.insert i (e, Just t)) d
        Nothing -> return $ updateVars (Map.insert i (Nothing, Just t)) d
