module Grammar
    (
    ) where

import Data.List
import Scan

data Grammar = Grammar Symbol SymbolSet deriving (Show)

getHead :: Grammar -> Symbol
getHead (Grammar head _) = head

genGrammar :: String -> [Symbol] -> Grammar
genGrammar name body = Grammar (NonTerm name) (SymbolSet body)

extend :: Grammar -> [Grammar] -> [Grammar]
extend start@(Grammar (NonTerm name) _) xs = Grammar (NonTerm "start") (SymbolSet [NonTerm name]) : start : xs

data AnalysisTree =
    Node [AnalysisTree]
  | Leaf Token

parse :: [Grammar] -> [Token] -> AnalysisTree
parse _ [] = Node []
parse grammars tokens = Node []

data Symbol =
    Term Term
  | NonTerm String
    deriving (Eq, Show)

data SymbolSet = SymbolSet [Symbol] deriving (Show)

data Action =
    Shift Int
  | Reduce SymbolSet
  | Accept

action :: [Grammar] -> Int -> Maybe Action
goto :: [Grammar] -> Int -> Maybe Int

---------- first ----------

first :: [Grammar] -> [(Symbol, [Symbol])]
first [] = []
first (x:xs) = nub $ (getHead x, first' nullable x) : first xs
  where nullable = (`elem` (nulls (x:xs)))

first' :: (Symbol -> Bool) -> Grammar -> [Symbol]
first' f (Grammar _ (SymbolSet [])) = []
first' f (Grammar _ (SymbolSet (x@(Term _):_))) = [x]
first' f (Grammar head (SymbolSet body)) =
  let
    ys = takeUpToNot f body
  in
    filter isTerm ys
  where
    isTerm :: Symbol -> Bool
    isTerm (Term _) = True
    isTerm _ = False

takeUpToNot :: (a -> Bool) -> [a] -> [a]
takeUpToNot f xs =
  let
    (l, r) = span f xs
  in
    case r of
      [] -> l
      otherwise -> l ++ [head r]

---------- nulls ----------

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
