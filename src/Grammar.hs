module Grammar
    (
    ) where

import Data.List

data Grammar = Grammar Symbol SymbolSet deriving (Show)

getHead :: Grammar -> Symbol
getHead (Grammar head _) = head

data Symbol =
    Term Term
  | NonTerm String
    deriving (Eq, Show)

data Term =
    Num
  | Ident
    deriving (Eq, Show)

data SymbolSet = SymbolSet [Symbol] deriving (Show)

nulls :: [Grammar] -> [Symbol]
nulls [] = []
nulls gs =
  let
    (tg', fg) = partition justNull gs
    tg = map getHead tg'
  in
    converge (nulls' fg) tg

justNull :: Grammar -> Bool
justNull (Grammar _ (SymbolSet [])) = True
justNull _ = False

nulls' :: [Grammar] -> [Symbol] -> [Symbol]
nulls' [] ns = ns
nulls' ((Grammar x (SymbolSet ss)):xs) ns
  | all (\x -> willBeNull x ns) ss = nulls' xs (x:ns)
  | otherwise = nulls' xs ns

willBeNull :: Symbol -> [Symbol] -> Bool
willBeNull (Term _) xs = False
willBeNull sym xs = sym `elem` xs

converge :: Eq a => (a -> a) -> a -> a
converge f y = let r = f y in
  if r == y then
    r
  else
    converge f r
