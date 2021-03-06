module Parser
    ( parse
    , extend
    , Grammar
    , NonTerm(Var)
    , Symbol
    , Symbol1(..)
    , (>:>)
    , refer
    , (|||)
    , nulls
    ) where

import Safe

import Prelude hiding (head)

import Control.Arrow hiding (first, (|||))
import qualified Control.Monad.State.Lazy as StateM
import Data.Bifunctor (Bifunctor, bimap)
import Data.Foldable
import Data.List hiding (head)
import qualified Data.List as List (head)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Tuple

import qualified Inter
import Scanner

---------- Data structures ----------

data Grammar = Grammar
  { startRule    :: (Rule, SemanticRule)
  , grammarRules :: Map.Map Rule SemanticRule
  }

instance Show Grammar where
  show x = "(Grammar " ++ (show . fst . startRule) x ++ " " ++ (show . Map.keys . grammarRules) x ++ ")"

data NonTerm =
    Var String
  | Start
    deriving (Eq, Show, Ord)

data Rule = Rule NonTerm [Symbol]
  deriving (Eq, Show, Ord)

type Symbol = Symbol1 Term NonTerm

data Symbol1 a b =
    Term a
  | NonTerm b
    deriving (Eq, Show, Ord)

symbol1 :: (a -> c) -> (b -> c) -> Symbol1 a b -> c
symbol1 f _ (Term a) = f a
symbol1 _ g (NonTerm b) = g b

instance Bifunctor Symbol1 where
  bimap f _ (Term a) = Term (f a)
  bimap _ g (NonTerm b) = NonTerm (g b)

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

type LookAhead = End Term

data End a =
    Middle a
  | End
    deriving (Eq, Show, Ord)

newtype State = State Int
  deriving (Eq, Show, Ord)

instance Enum State where
  toEnum = State
  fromEnum (State n) = n

data Action =
    Shift State
  | Reduce Rule
  | Accept
    deriving Show


extend :: NonTerm -> Map.Map Rule SemanticRule -> Grammar
extend start rules = Grammar
  { startRule = (Rule Start [NonTerm start], \xs -> (Inter.NOP, xs`at`0, Inter.Nil))
  , grammarRules = rules
  }

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
    rules = Map.keys $ grammarRules grammar
    s = Map.fromAscList . zip [State 0..] . Set.toAscList $ states (fst $ startRule grammar) rules
    s0 = fst . Map.elemAt 0 $ Map.filter (Set.member $ Item (fst $ startRule grammar) 0 End) s
    f = action (gotoItems rules) (fst $ startRule grammar) s
    g = goto (gotoItems rules) s
    m = semRuleOf $ grammarRules grammar
  in
    parse' m f g s0 $ map Middle tokens ++ [End]

action :: (Items -> Symbol -> Items) -> Rule -> Map.Map State Items -> State -> End Token -> Action
action gotoF start states n token =
  case token of
    End -> atEnd gotoF start states state
    _ -> action' gotoF states state token
  where
    state :: Items
    state = fromJust $ Map.lookup n states

atEnd :: (Items -> Symbol -> Items) -> Rule -> Map.Map State Items -> Items -> Action
atEnd gotoF start states current
  | Item start 1 End `Set.member` current = Accept
  | otherwise = action' gotoF states current End

action' :: (Items -> Symbol -> Items) -> Map.Map State Items -> Items -> End Token -> Action
action' gotoF states current token
  | not $ Set.null matchReduce
    = reduce . Set.findMin $ matchReduce
  | not $ Set.null matchShift
    = fromJust $ fmap Shift . getID states . gotoF current $ toSymbol token
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
    matchShift = Set.filter (maybe False ((`eqLaToken` token) . Middle) . next) current

    toSymbol :: End Token -> Symbol
    toSymbol (Middle t) = Term $ getTerm t
    toSymbol End = error "unexpected End"

getID :: Map.Map State Items -> Items -> Maybe State
getID states items = headMay . Map.keys . Map.filter (== items) $ states

eqLaToken :: LookAhead -> End Token -> Bool
eqLaToken End End = True
eqLaToken End _ = False
eqLaToken _ End = False
eqLaToken (Middle term) (Middle t) = term == getTerm t

goto :: (Items -> Symbol -> Items) -> Map.Map State Items -> State -> NonTerm -> State
goto gotoF states n = fromJust . getID states . gotoF state . NonTerm
  where
    state :: Items
    state = fromJust $ Map.lookup n states

-- | Generated intermediate codes.
type ParseState = [Inter.Quad]

data ParseStack = ParseStack
  { addrSupply      :: Inter.Addr              -- ^ Address supplier.
  , stateStack      :: NonEmpty.NonEmpty State -- ^ State Stack.
  , shiftedOperands :: [Inter.Operand]         -- ^ Shifted intermediate operands some of which may have been reduced.
  }

parseStack :: State -> ParseStack
parseStack state = ParseStack
  { addrSupply = 1
  , stateStack = state NonEmpty.:| []
  , shiftedOperands = []
  }

currentState :: ParseStack -> State
currentState = NonEmpty.head . stateStack

push :: State -> StateM.State ParseStack ()
push state = StateM.modify $ \ps -> ps { stateStack = state NonEmpty.<| stateStack ps }

newAddr :: StateM.State ParseStack Inter.Addr
newAddr = do
  addr <- StateM.gets addrSupply
  StateM.modify $ \ps -> ps { addrSupply = 1 + addr }
  return addr

setStack :: NonEmpty.NonEmpty State -> StateM.State ParseStack ()
setStack stack = StateM.modify $ \ps -> ps { stateStack = stack }

setOperands :: [Inter.Operand] -> StateM.State ParseStack ()
setOperands ops = StateM.modify $ \ps -> ps { shiftedOperands = ops }

parse' :: (Rule -> SemanticRule) -> (State -> End Token -> Action) -> (State -> NonTerm -> State) -> State -> [End Token] -> [Inter.Quad]
parse' m f g s0 = flip StateM.evalState (parseStack s0) . foldlM buildTree []
  where
    buildTree :: ParseState -> End Token -> StateM.State ParseStack ParseState
    buildTree quads token = do
      state <- StateM.gets currentState
      case f state token of
        Accept -> return quads
        Shift n -> do
          push n
          ops <- StateM.gets shiftedOperands
          setOperands $ tokenToOperand (fromToken token) : ops
          return quads
        Reduce rule -> do
          stack <- StateM.gets stateStack
          addr <- newAddr
          let bodyLen = length . getBody $ rule
          setStack . app $ ((NonEmpty.:|) <<< uncurry g <<< List.head *** getHead) &&& fst $ (NonEmpty.drop bodyLen stack, rule)
          let
            h :: [Inter.Operand] -> StateM.State ParseStack ParseState
            h = flip buildTree token . (: quads) . Inter.toQuad (Inter.Point addr) . m rule . reverse

            u :: [Inter.Operand] -> StateM.State ParseStack ()
            u = setOperands <<< (Inter.At addr :)
          flip runKleisli shiftedOperands $ arr snd <<< Kleisli u *** Kleisli h <<< arr (swap . splitAt bodyLen) <<< Kleisli StateM.gets

    fromToken :: End Token -> Token
    fromToken (Middle token) = token
    fromToken End = error "fromToken: unexpected End"

    tokenToOperand :: Token -> Inter.Operand
    tokenToOperand = Inter.Const . fromRight . fromNum

    fromRight :: Either a b -> b
    fromRight (Right b) = b
    fromRight _ = error "fromRight: Left value"

---------- States ----------

states :: Rule -> [Rule] -> Set.Set Items
states start rules = converge (states' rules) . Set.singleton . closure rules . Set.singleton $ Item start 0 End

states' :: [Rule] -> Set.Set Items -> Set.Set Items
states' rules c = Set.union c . Set.fromList $ do
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

gotoItems :: [Rule] -> Items -> Symbol -> Items
gotoItems rules items sym = closure rules . Set.map inc $ Set.filter hasNext items
  where
    hasNext :: Item -> Bool
    hasNext (Item rule n _) | length (getBody rule) <= n = False
    hasNext item = nextSym item == sym

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
                  | la <- la1 . map Middle . Set.toList . firstOfSymbols rules $ afterNext
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
firstOfSymbols rules symbols = Set.unions . map m $ takeUpToNot (nullable rules, symbols)
  where
    m :: Symbol -> Set.Set Term
    m = symbol1 Set.singleton $ \t -> Map.findWithDefault Set.empty t $ firstS rules

firstS :: [Rule] -> Map.Map NonTerm (Set.Set Term)
firstS rules = converge (first rules) Map.empty

first :: [Rule] -> Map.Map NonTerm (Set.Set Term) -> Map.Map NonTerm (Set.Set Term)
first rules stack = Map.fromListWith Set.union [ (head, first' (nullable rules) body stack) | (Rule head body) <- rules ]

first' :: (Symbol -> Bool) -> [Symbol] -> Map.Map NonTerm (Set.Set Term) -> Set.Set Term
first' _ [] _ = Set.empty
first' _ (Term x:_) _ = Set.singleton x
first' f body stack =
  Set.unions $
    symbol1 Set.singleton
            (\t -> Map.findWithDefault Set.empty t stack) <$>
            takeUpToNot (f, body)

takeUpToNot :: (a -> Bool, [a]) -> [a]
takeUpToNot = app <<< (++) *** maybeToList . headMay <<< uncurry span

---------- Nulls ----------

nullable :: [Rule] -> Symbol -> Bool
nullable rules (NonTerm x) = x `elem` nulls rules
nullable _ _ = False

nulls :: [Rule] -> [NonTerm]
nulls = app <<< converge . nulls' *** map getHead <<< swap <<< partition justNull

justNull :: Rule -> Bool
justNull = null . getBody

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
