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

data Rule = Rule NonTerm [Symbol] deriving (Eq, Show, Ord)

data Symbol = Term Term | NonTerm NonTerm deriving (Eq, Show, Ord)

data AST = Node [AST] | Leaf Token deriving Show

-- LR(1) Item
type Items = Set.Set Item
data Item = Item Rule Int LookAhead deriving (Eq, Show, Ord)

data LookAhead = LookAhead Term | EndPoint deriving (Eq, Show, Ord)

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

---------- closure ----------

closure :: [Rule] -> Items -> Items
closure rules items = converge (closure' rules) items

closure' :: [Rule] -> Items -> Items
closure' rules items = Set.foldl Set.union Set.empty $ Set.map (closeItem rules) items

closeItem :: [Rule] -> Item -> Items
closeItem rules item@(Item (Rule _ body) n _)
  | length body <= n = Set.singleton item
closeItem rules item = Set.fromList $ item : concat [ [ Item rule 0 la | la <- ((la1 . map LookAhead . Set.toList . firstOfSymbols rules) afterNext) ] | rule@(Rule head body) <- rules, (NonTerm head) == next ]
  where
    next :: Symbol
    next = let (Item (Rule _ body) n _) = item in body !! n
    afterNext :: [Symbol]
    afterNext = let (Item (Rule _ body) n _) = item in drop (n+1) body
    la1 :: [LookAhead] -> [LookAhead]
    la1 x
      | all (nullable rules) afterNext = let (Item _ _ a) = item in a : x
      | otherwise = x

---------- first ----------

firstOfSymbols :: [Rule] -> [Symbol] -> Set.Set Term
firstOfSymbols rules symbols = Set.unions $ map m $ takeUpToNot (nullable rules) symbols
  where
    m :: Symbol -> Set.Set Term
    m (NonTerm t) = maybe Set.empty id $ Map.lookup t $ firstS rules
    m (Term t) = Set.singleton t

firstS :: [Rule] -> Map.Map NonTerm (Set.Set Term)
firstS rules = converge (first rules) Map.empty

first :: [Rule] -> Map.Map NonTerm (Set.Set Term) -> Map.Map NonTerm (Set.Set Term)
first rules stack = Map.fromListWith Set.union [ (head, first' (nullable rules) body stack) | (Rule head body) <- rules ]

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

nullable :: [Rule] -> Symbol -> Bool
nullable rules (NonTerm x) = x `elem` (nulls rules)
nullable _ _ = False

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

exampleGrammar2 :: Grammar
exampleGrammar2 = Grammar (Var "S")
  [ "S" >:> [ refer "C", refer "C" ]
  , "C" >:> [ Term Add, refer "C" ]
  , "C" >:> [ Term Sub ]
  ]
