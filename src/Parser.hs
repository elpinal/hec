module Parser
    (
    ) where

import Data.List

import Scan (Token(..), Term(..))

---------- Parser ----------

data Grammar = Grammar NonTerm [Rule] deriving Show

data NonTerm = Var String | Start deriving (Eq, Show)

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


---------- nulls ----------

nulls :: Grammar -> [NonTerm]
nulls (Grammar _ []) = []
nulls (Grammar start rules) =
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
  [ "expr" >:> [ Term Num, Term Add, refer "expr" ]
  , "expr" >:> [ refer "term" ]
  , "term" >:> [ Term Sub, Term Num, refer "term" ]
  , "term" >:> []
  ]
