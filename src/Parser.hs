module Parser
    (
    ) where

import Scan (Token, Term)

---------- Parser ----------

data Grammar = Grammar NonTerm [Rule]

data NonTerm = Var String | Start

data Rule = Rule NonTerm [Symbol]

data Symbol = Term Term | NonTerm NonTerm

data AST = Node [AST] | Leaf Token

data State = State Int

data Action =
    Shift Int
  | Reduce Rule
  | Accept

extend :: Grammar -> Grammar
extend (Grammar start rules) = Grammar Start ((Rule Start [NonTerm start]):rules)

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
goto :: Grammar -> String -> Token -> Int

makeAST :: [Rule] -> [Token] -> AST
makeAST steps tokens = Node []

nulls :: Grammar -> [NonTerm]
nulls [] = []
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
  | (NonTerm head) `notElem` ns && all f syms = nulls' rules (head:ns)
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
