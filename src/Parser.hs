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

import qualified Control.Monad.State.Lazy as StateM
import Data.Foldable
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set

import qualified Inter
import Scanner

---------- Data structures ----------

data Grammar = Grammar NonTerm (Map.Map Rule SemanticRule)

instance Show Grammar where
  show (Grammar x xs) = "(Grammar " ++ show x ++ " " ++ (show . Map.keys) xs ++ ")"

data NonTerm =
    Var String
  | Start
    deriving (Eq, Show, Ord)

data Rule = Rule NonTerm [Symbol]
  deriving (Eq, Show, Ord)

data Symbol =
    Term Term
  | NonTerm NonTerm
    deriving (Eq, Show, Ord)

type SemanticRule = [Inter.Operand] -> Inter.Triple

semRuleOf :: Map.Map Rule SemanticRule -> Rule -> SemanticRule
semRuleOf ruleSet prodRule = Map.findWithDefault (error msg) prodRule ruleSet
  where
    msg :: String
    msg = "unexpected error: the semantic rule corresponding to " ++ show prodRule

-- LR(1) Item
type Items = Set.Set Item
data Item = Item Rule Int LookAhead
  deriving (Eq, Show, Ord)

data LookAhead =
    LookAhead Term
  | EndPoint
    deriving (Eq, Show, Ord)

newtype State = State Int
  deriving Show

data Action =
    Shift Int
  | Reduce Rule
  | Accept
    deriving Show

data Token' =
    Token' Token
  | EndToken
    deriving (Eq, Show, Ord)

extend :: Grammar -> Grammar
extend (Grammar start rules) = Grammar Start $ Map.insert (Rule Start [NonTerm start]) (\xs -> (Inter.NOP, xs`at`0, Inter.Nil)) rules

getRules :: Grammar -> Map.Map Rule SemanticRule
getRules (Grammar _ rules) = rules

getHead :: Rule -> NonTerm
getHead (Rule head _) = head

getBody :: Rule -> [Symbol]
getBody (Rule _ body) = body

getNextSym :: Item -> Maybe Symbol
getNextSym (Item (Rule _ body) n _) = body `atMay` n

nextSym :: Item -> Symbol
nextSym (Item (Rule _ body) n _) = body `at` n

---------- Parse ----------

parse :: Grammar -> [Token] -> [Inter.Quad]
parse _ [] = []
parse grammar tokens =
  let
    rulesSems = getRules grammar
    rules = Map.keys rulesSems
    start = getStart rules
    s = Map.fromAscList . zip [0..] . Set.toAscList $ states rules
    s0 = fst . Map.elemAt 0 $ Map.filter (Set.member $ Item (getStart rules) 0 EndPoint) s
    f = action (gotoItems rules) start s
    g = goto (gotoItems rules) s
    m = semRuleOf rulesSems
  in
    parse' m f g s0 $ map Token' tokens ++ [EndToken]

action :: (Items -> Symbol -> Maybe Items) -> Rule -> Map.Map Int Items -> State -> Token' -> Action
action gotoF start states (State n) token =
  case token of
    EndToken -> atEnd gotoF start states state
    _ -> action' gotoF states state token
  where
    state :: Items
    state = fromJust $ Map.lookup n states

atEnd :: (Items -> Symbol -> Maybe Items) -> Rule -> Map.Map Int Items -> Items -> Action
atEnd gotoF start states current
  | Item start 1 EndPoint `Set.member` current = Accept
  | otherwise = action' gotoF states current EndToken

action' :: (Items -> Symbol -> Maybe Items) -> Map.Map Int Items -> Items -> Token' -> Action
action' gotoF states current token
  | not $ Set.null matchReduce
    = reduce . Set.findMin $ matchReduce
  | not $ Set.null matchShift
    = fromJust . fmap Shift $ getID states =<< gotoF current (fromToken' token)
  | otherwise
    = error $ "unexpected error: " ++ show token
  where
    matchReduce :: Items
    matchReduce = Set.filter f current

    f :: Item -> Bool
    f (Item rule n la) = la `eqLaToken` token && length (getBody rule) == n

    reduce :: Item -> Action
    reduce (Item rule _ _) = Reduce rule

    next :: Item -> Maybe Term
    next item = getNextSym item >>= fromTerm

    fromTerm :: Symbol -> Maybe Term
    fromTerm (Term t) = return t
    fromTerm _ = Nothing

    matchShift :: Items
    matchShift = Set.filter (maybe False ((`eqLaToken` token) . LookAhead) . next) current

    fromToken' :: Token' -> Symbol
    fromToken' (Token' t) = Term $ getTerm t

getID :: Map.Map Int Items -> Items -> Maybe Int
getID states items = headMay . Map.keys . Map.filter (== items) $ states

eqLaToken :: LookAhead -> Token' -> Bool
eqLaToken EndPoint EndToken = True
eqLaToken EndPoint _ = False
eqLaToken _ EndToken = False
eqLaToken (LookAhead term) (Token' t) = term == getTerm t

goto :: (Items -> Symbol -> Maybe Items) -> Map.Map Int Items -> State -> NonTerm -> State
goto gotoF states (State n) nt =
  maybe (error $ "unexpected " ++ show nt)
        (State . fromJust . getID states) $
        gotoF state $ NonTerm nt
  where
    state :: Items
    state = fromJust $ Map.lookup n states

type ParseState = ([Int], [Inter.Quad], [Inter.Operand])

parse' :: (Rule -> SemanticRule) -> (State -> Token' -> Action) -> (State -> NonTerm -> State) -> Int -> [Token'] -> [Inter.Quad]
parse' m f g s0 tokens = snd' . flip StateM.evalState 1 $ foldlM buildTree ([s0], [], []) tokens
  where
    buildTree :: ParseState -> Token' -> StateM.State Inter.Addr ParseState
    buildTree (state:xs, quads, passed) token =
      case f (State state) token of
        Accept -> return (xs, quads, passed)
        Shift n -> return (n:state:xs, quads, tokenToOperand (fromToken token):passed)
        Reduce rule -> do
          addr <- StateM.get
          StateM.put $ addr + 1
          let
            bodyLen = length . getBody $ rule
            (ps1, ps2) = splitAt bodyLen passed
            semRule = m rule
            (s:ss) = drop bodyLen (state:xs)
            result = Inter.Point addr
            triple = semRule $ reverse ps1
            quad = Inter.toQuad result triple
          flip buildTree token
                         ( (getIdx . g (State s)) (getHead rule) : s : ss
                         , quad : quads
                         , Inter.At addr : ps2
                         )

    fromToken :: Token' -> Token
    fromToken (Token' token) = token
    fromToken EndToken = error "unexpected EndToken"

    getIdx :: State -> Int
    getIdx (State n) = n

    tokenToOperand :: Token -> Inter.Operand
    tokenToOperand = Inter.Const . fromRight . fromNum

    fromRight :: Either a b -> b
    fromRight (Right b) = b
    fromRight _ = error "fromRight: Left value"

    snd' :: (a, b, c) -> b
    snd' (_, x, _) = x

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
    syms' (Rule Start _) = []
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
    filterFunc :: Item -> Bool
    filterFunc (Item (Rule _ body) n _) | length body <= n = False
    filterFunc item = nextSym item == sym

    inc :: Item -> Item
    inc (Item r n l) = Item r (n + 1) l

---------- Closure ----------

closure :: [Rule] -> Items -> Items
closure rules = converge $ closure' rules

closure' :: [Rule] -> Items -> Items
closure' rules items = Set.foldl Set.union Set.empty $ Set.map (closeItem rules) items

closeItem :: [Rule] -> Item -> Items
closeItem _ item@(Item (Rule _ body) n _)
  | length body <= n = Set.singleton item
closeItem rules item = Set.fromList $
  item : concat [ [ Item rule 0 la
                  | la <- la1 . map LookAhead . Set.toList . firstOfSymbols rules $ afterNext
                  ]
                | rule@(Rule head _) <- rules, NonTerm head == nextSym item
                ]
  where
    afterNext :: [Symbol]
    afterNext = let (Item (Rule _ body) n _) = item in drop (n + 1) body

    la1 :: [LookAhead] -> [LookAhead]
    la1 x
      | all (nullable rules) afterNext = let (Item _ _ a) = item in a : x
      | otherwise = x

---------- First ----------

firstOfSymbols :: [Rule] -> [Symbol] -> Set.Set Term
firstOfSymbols rules symbols = Set.unions . map m $ takeUpToNot (nullable rules) symbols
  where
    m :: Symbol -> Set.Set Term
    m (NonTerm t) = fromMaybe Set.empty . Map.lookup t $ firstS rules
    m (Term t) = Set.singleton t

firstS :: [Rule] -> Map.Map NonTerm (Set.Set Term)
firstS rules = converge (first rules) Map.empty

first :: [Rule] -> Map.Map NonTerm (Set.Set Term) -> Map.Map NonTerm (Set.Set Term)
first rules stack = Map.fromListWith Set.union [ (head, first' (nullable rules) body stack) | (Rule head body) <- rules ]

first' :: (Symbol -> Bool) -> [Symbol] -> Map.Map NonTerm (Set.Set Term) -> Set.Set Term
first' _ [] _ = Set.empty
first' _ (Term x:_) _ = Set.singleton x
first' f body stack =
  let
    xs = takeUpToNot f body
  in Set.unions $ do
    x <- xs
    return $ case x of
      Term t -> Set.singleton t
      NonTerm t -> fromMaybe Set.empty $ Map.lookup t stack

takeUpToNot :: (a -> Bool) -> [a] -> [a]
takeUpToNot f xs =
  let
    (l, r) = span f xs
  in
    case r of
      [] -> l
      _ -> l ++ [head r]

---------- Nulls ----------

nullable :: [Rule] -> Symbol -> Bool
nullable rules (NonTerm x) = x `elem` nulls rules
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
nulls' (Rule head syms:rules) ns
  | head `notElem` ns && all f syms = nulls' rules $ head : ns
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
