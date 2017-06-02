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

extend :: Grammar -> Grammar

parse :: Grammar -> [Token] -> AST
