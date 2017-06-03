module Parser
    (
    ) where

import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

import Scan (Token(..), Term(..))

---------- Parser ----------

data Grammar = Grammar NonTerm [Rule] deriving Show

data NonTerm = Var String | Start deriving (Eq, Show, Ord)

data Rule = Rule NonTerm [Symbol] deriving Show

data Symbol = Term Term | NonTerm NonTerm deriving Show

data AST = Node [AST] | Leaf Token deriving Show

-- LR(1) Item
type Items = [Item]
data Item = Item Rule Int LookAhead deriving Show

data LookAhead = LookAhead Token | EndPoint deriving Show

data State = State Int deriving Show

data Action =
    Shift Int
  | Reduce Rule
  | Accept
    deriving Show

extend :: Grammar -> Grammar
extend (Grammar start rules) = Grammar Start ((Rule Start [NonTerm start]):rules)

getHead :: Rule -> NonTerm
getHead (Rule head body) = head

parse :: Grammar -> [Token] -> AST
parse _ [] = Node []
parse grammar tokens =
  let
    f = action grammar
    g = goto grammar
    steps = parse' f g tokens
  in
    makeAST steps tokens

action :: Grammar -> State -> Token -> Action
action grammar state token = Accept

goto :: Grammar -> State -> Token -> State
goto grammar state token = state

parse' :: (State -> Token -> Action) -> (State -> Token -> State) -> [Token] -> [Rule]
parse' f g tokens = []

makeAST :: [Rule] -> [Token] -> AST
makeAST steps tokens = Node []

---------- first----------

firstS :: [Rule] -> Map.Map NonTerm (Set.Set Term)
firstS rules = converge (first rules) Map.empty

first :: [Rule] -> Map.Map NonTerm (Set.Set Term) -> Map.Map NonTerm (Set.Set Term)
first rules stack = Map.fromListWith Set.union [ (head, first' nullable body stack) | (Rule head body) <- rules ]
  where
    nullable (NonTerm x) = x `elem` (nulls rules)
    nullable (Term _) = False

first' :: (Symbol -> Bool) -> [Symbol] -> Map.Map NonTerm (Set.Set Term) -> Set.Set Term
first' f [] _ = Set.empty
first' f ((Term x):_) _ = Set.singleton x
first' f body stack =
  let
    xs = takeUpToNot f body
  in Set.unions $ do
    x <- xs
    return $ case x of
      Term t -> Set.singleton t
      NonTerm t -> maybe Set.empty id $ Map.lookup t stack
  where
    isTerm :: Symbol -> Bool
    isTerm (Term t) = True
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

nulls :: [Rule] -> [NonTerm]
nulls [] = []
nulls rules =
  let
    (nu, pend) = partition justNull rules
    ns = map getHead nu
  in
    converge (nulls' pend) ns

justNull :: Rule -> Bool
justNull (Rule _ []) = True
justNull _ = False

nulls' :: [Rule] -> [NonTerm] -> [NonTerm]
nulls' [] ns = ns
nulls' ((Rule head syms):rules) ns
  | head `notElem` ns && all f syms = nulls' rules (head:ns)
  | otherwise = nulls' rules ns
    where
      f (NonTerm t) = t `elem` ns
      f _ = False

converge :: Eq a => (a -> a) -> a -> a
converge f x = let r = f x in
  if r == x then
    r
  else
    converge f r

---------- Examples ----------

(>:>) :: String -> [Symbol] -> Rule
head >:> body = Rule (Var head) body

refer :: String -> Symbol
refer = NonTerm . Var

exampleGrammar :: Grammar
exampleGrammar = Grammar (Var "expr")
  [ "expr" >:> [ refer "term", Term Num, Term Add, refer "expr" ]
  , "expr" >:> [ refer "term" ]
  , "term" >:> [ refer "factor", Term Sub, Term Num, refer "term" ]
  , "term" >:> []
  , "factor" >:> [ refer "expr", refer "term", Term Ident ]
  ]
