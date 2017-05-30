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
    (nu, pend) = partition justNull gs
    ns = map getHead nu
  in
    converge (nulls' pend) ns

justNull :: Grammar -> Bool
justNull (Grammar _ (SymbolSet [])) = True
justNull _ = False

nulls' :: [Grammar] -> [Symbol] -> [Symbol]
nulls' [] ns = ns
nulls' ((Grammar x (SymbolSet ss)):xs) ns
  | x `notElem` ns && all (`elem` ns) ss = nulls' xs (x:ns)
  | otherwise = nulls' xs ns

converge :: Eq a => (a -> a) -> a -> a
converge f x = let r = f x in
  if r == x then
    r
  else
    converge f r
