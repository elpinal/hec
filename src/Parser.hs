module Parser
    ( parse
    , extend
    , Grammar(..)
    , NonTerm(..)
    , Symbol(..)
    , (>:>)
    , refer
    , (|||)
    ) where

import Safe

import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set

import qualified Inter
import Scanner (Token(..), Term(..))

---------- Data structures ----------

data Grammar = Grammar NonTerm [(Rule, SemanticRule)]

instance Show Grammar where
  show (Grammar x xs) = "(Grammar " ++ show x ++ " " ++ (show . map fst) xs

data NonTerm = Var String | Start deriving (Eq, Show, Ord)

data Rule = Rule NonTerm [Symbol] deriving (Eq, Show, Ord)

data Symbol = Term Term | NonTerm NonTerm deriving (Eq, Show, Ord)

type SemanticRule = [Inter.Operand] -> Inter.Triple

semRuleOf :: [(Rule, SemanticRule)] -> Rule -> SemanticRule
semRuleOf ruleSet prodRule =
  case find (\(p, s) -> if p == prodRule then True else False) ruleSet of
    Just (p, s) -> s
    Nothing -> error $ "unexpected error: the semantic rule corresponding to " ++ show prodRule

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
extend (Grammar start rules) = Grammar Start ((Rule Start [NonTerm start])|||(\xs -> (Inter.NOP, xs`at`0, Inter.Nil)):rules)

getHead :: Rule -> NonTerm
getHead (Rule head body) = head

getBody :: Rule -> [Symbol]
getBody (Rule _ body) = body

---------- Parse ----------

parse :: Grammar -> [Token] -> [Inter.Quad]
parse _ [] = []
parse grammar tokens =
  let
    (Grammar _ rulesSems) = grammar
    rules = map fst rulesSems
    start = getStart rules
    s = Map.fromAscList . zip [0..] . Set.toAscList $ states rules
    s0 = fst . Map.elemAt 0 $  Map.filter (Set.member $ Item (getStart rules) 0 EndPoint) s
    f = action (gotoItems rules) start s
    g = goto (gotoItems rules) s
    m = semRuleOf rulesSems
  in
    parse' m f g s0 $ map Token' tokens ++ [EndToken]

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
  | not $ Set.null matchReduce
    = Reduce . getRule . head . Set.toList $ matchReduce
  | not $ Set.null matchShift
    = fromJust $ return . Shift =<< getID states =<< (gotoF current $ fromToken' token)
  | otherwise
    = error $ "unexpected error: " ++ show token
  where
    matchReduce :: Items
    matchReduce = Set.filter (\(Item rule n la) -> la `eqLaToken` token && length (getBody rule) == n) current
    getRule :: Item -> Rule
    getRule (Item rule _ _) = rule
    next :: Item -> Maybe Term
    next item =
      let (Item (Rule _ body) n _) = item in
        case body `atMay` n of
          Just (Term t) -> Just t
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

parse' :: (Rule -> SemanticRule) -> (State -> Token' -> Action) -> (State -> NonTerm -> State) -> Int -> [Token'] -> [Inter.Quad]
parse' m f g s0 tokens = snd' $ foldl buildTree ([s0], [], [], [1..]) tokens
  where
    buildTree :: ([Int], [Inter.Quad], [Inter.Operand], [Inter.Addr]) -> Token' -> ([Int], [Inter.Quad], [Inter.Operand], [Inter.Addr])
    buildTree ((state:xs), quads, passed, addrNumbers) token = case f (State state) token of
      Accept -> (xs, quads, passed, addrNumbers)
      Shift n -> ((n:state:xs), quads, (tokenToOperand (fromToken token):passed), addrNumbers)
      Reduce rule -> let
        bodyLen = length . getBody $ rule
        (ps1, ps2) = splitAt bodyLen passed
        semRule = m rule
        (s:ss) = drop bodyLen (state:xs)
        addr = head addrNumbers
        result = Inter.Point addr
        triple = semRule $ reverse ps1
        quad = Inter.toQuad result triple
        in
        buildTree (((fromState . g (State s)) (getHead rule) : s : ss), (quad:quads), (Inter.At addr:ps2), (drop 1 addrNumbers)) token
    fromToken :: Token' -> Token
    fromToken (Token' token) = token
    fromToken EndToken = error "unexpected EndToken"
    fromState :: State -> Int
    fromState (State n) = n
    tokenToOperand :: Token -> Inter.Operand
    tokenToOperand (Token (Num, val)) = Inter.Const . read $ val
    snd' :: (a, b, c, d) -> b
    snd' (a, b, c, d) = b

---------- States ----------

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
    next item = let (Item (Rule _ body) n _) = item in body `at` n
    filterFunc :: Item -> Bool
    filterFunc (Item (Rule _ body) n _) | length body <= n = False
    filterFunc item = next item == sym
    inc :: Item -> Item
    inc (Item r n l) = Item r (n+1) l

---------- Closure ----------

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
    next = let (Item (Rule _ body) n _) = item in body `at` n
    afterNext :: [Symbol]
    afterNext = let (Item (Rule _ body) n _) = item in drop (n+1) body
    la1 :: [LookAhead] -> [LookAhead]
    la1 x
      | all (nullable rules) afterNext = let (Item _ _ a) = item in a : x
      | otherwise = x

---------- First ----------

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

---------- Nulls ----------

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

---------- Useful functions ----------

(>:>) :: String -> [Symbol] -> Rule
head >:> body = Rule (Var head) body

refer :: String -> Symbol
refer = NonTerm . Var

infix 8 |||
(|||) :: Rule -> SemanticRule -> (Rule, SemanticRule)
rule ||| sem = (rule, sem)
