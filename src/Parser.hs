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
extend (Grammar start rules) = Grammar Start ((Rule Start [NonTerm start]):rules)

parse :: Grammar -> [Token] -> AST
