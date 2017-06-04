module Parser
    (
    ) where

import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
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

data Token' = Token' Token | EndToken deriving (Eq, Show, Ord)

extend :: Grammar -> Grammar
extend (Grammar start rules) = Grammar Start ((Rule Start [NonTerm start]):rules)

getHead :: Rule -> NonTerm
getHead (Rule head body) = head

parse :: Grammar -> [Token] -> AST
parse _ [] = Node []
parse grammar tokens =
  let
    (Grammar _ rules) = grammar
    start = getStart rules
    s = Map.fromAscList . zip [0..] . Set.toAscList $ states rules
    f = action (gotoItems rules) start s
    g = goto (gotoItems rules) s
    steps = parse' f g $ map Token' tokens ++ [EndToken]
  in
    makeAST steps tokens

action :: (Items -> Symbol -> Maybe Items) -> Rule -> Map.Map Int Items -> State -> Token' -> Action
action gotoF start states (State n) token =
  let
    state = fromJust $ Map.lookup n states
  in
    case token of
      EndToken -> atEnd gotoF start states state
      otherwise -> action' gotoF states state token

atEnd :: (Items -> Symbol -> Maybe Items) -> Rule -> Map.Map Int Items -> Items -> Action
atEnd gotoF start states current
  | (Item start 1 EndPoint) `Set.member` current = Accept
  | otherwise = action' gotoF states current EndToken

action' :: (Items -> Symbol -> Maybe Items) -> Map.Map Int Items -> Items -> Token' -> Action
action' gotoF states current token
  | 1 == Set.size matchReduce
    = Reduce . getRule . head . Set.toList $ matchReduce
  | 1 == Set.size matchShift
    = fromJust $ return . Shift =<< getID states =<< (gotoF current $ fromToken' token)
  where
    matchReduce :: Items
    matchReduce = Set.filter (\(Item _ _ la) -> la `eqLaToken` token) current
    getRule :: Item -> Rule
    getRule (Item rule _ _) = rule
    next :: Item -> Maybe Term
    next item =
      let (Item (Rule _ body) n _) = item in
        case body !! n of
          (Term t) -> Just t
          otherwise -> Nothing
    matchShift :: Items
    matchShift = Set.filter (\i -> fromMaybe False $ next i >>= return . (`eqLaToken` token) . LookAhead) current
    fromToken' :: Token' -> Symbol
    fromToken' (Token' (Token (term, _))) = Term term
    getID :: Map.Map Int Items -> Items -> Maybe Int
    getID states items = headMaybe . Map.keys . Map.filter (==items) $ states
    headMaybe :: [a] -> Maybe a
    headMaybe [] = Nothing
    headMaybe (x:xs) = Just x

eqLaToken :: LookAhead -> Token' -> Bool
eqLaToken EndPoint EndToken = True
eqLaToken EndPoint _ = False
eqLaToken _ EndToken = False
eqLaToken (LookAhead term) (Token' (Token (token, _))) = term == token

goto :: (Items -> Symbol -> Maybe Items) -> Map.Map Int Items -> State -> NonTerm -> State
goto gotoF states (State n) nt =
  let
    state = fromJust $ Map.lookup n states
    j = gotoF state (NonTerm nt)
  in
    case j of
      Nothing -> error $ "unexpected " ++ show nt
      (Just j') -> State . fromJust $ getID states j'
  where
    getID :: Map.Map Int Items -> Items -> Maybe Int
    getID states items = headMaybe . Map.keys . Map.filter (==items) $ states
    headMaybe :: [a] -> Maybe a
    headMaybe [] = Nothing
    headMaybe (x:xs) = Just x

parse' :: (State -> Token' -> Action) -> (State -> NonTerm -> State) -> [Token'] -> [Rule]
parse' f g tokens = []

makeAST :: [Rule] -> [Token] -> AST
makeAST steps tokens = Node []

---------- states ----------

states :: [Rule] -> Set.Set Items
states rules = converge (states' rules) . Set.singleton . closure rules . Set.singleton $ Item (getStart rules) 0 EndPoint

getStart :: [Rule] -> Rule
getStart (rule@(Rule Start _):_) = rule
getStart (_:xs) = getStart xs
getStart [] = error "grammar should be extended"

states' :: [Rule] -> Set.Set Items -> Set.Set Items
states' rules c = Set.union c . Set.fromList . catMaybes $ do
  items <- Set.toList c
  sym <- Set.toList syms
  return $ gotoItems rules items sym
    where
      syms :: Set.Set Symbol
      syms = Set.fromList . concat $ map syms' rules
      syms' :: Rule -> [Symbol]
      syms' (Rule Start body) = []
      syms' (Rule head body) = NonTerm head : body

---------- goto items ----------

gotoItems :: [Rule] -> Items -> Symbol -> Maybe Items
gotoItems rules items sym =
  let xs = Set.filter filterFunc items in
    if xs == Set.empty then
      Nothing
    else
      return . closure rules . Set.map inc $ xs
  where
    next :: Item -> Symbol
    next item = let (Item (Rule _ body) n _) = item in body !! n
    filterFunc :: Item -> Bool
    filterFunc (Item (Rule _ body) n _) | length body <= n = False
    filterFunc item = next item == sym
    inc :: Item -> Item
    inc (Item r n l) = Item r (n+1) l

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
