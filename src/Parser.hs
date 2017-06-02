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
