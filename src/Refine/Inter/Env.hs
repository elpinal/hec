module Refine.Inter.Env
  ( emptyDecls
  , scanDecls
  , updateVars
  ) where

import Control.Monad
import qualified Data.Map.Lazy as Map

import qualified Refine.AST as AST
import qualified Refine.Type.Syntactic as S

type Vars  = Map.Map String (AST.Expr, Maybe S.Type)
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

scanDecls :: [AST.Decl] -> Maybe Decls
scanDecls ds = foldl (>=>) return (map scanDecl ds) emptyDecls
  where
    scanDecl :: AST.Decl -> Decls -> Maybe Decls
    scanDecl (AST.VarDecl i e) d =
      if i `Map.member` vars d
        then Nothing
        else Just $ updateVars (Map.insert i (e, Nothing)) d
