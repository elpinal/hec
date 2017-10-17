module Refine.Kind where

data Kind =
    Star
  | KFun Kind Kind
  deriving (Eq, Ord, Show)
