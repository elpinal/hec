module Syntax
    (
    ) where

---------- LR(1) Syntax Analysis ----------

import Data.Monoid

data Term =
    Eps
  | Num
  | Ident
  | Add
  | Sub
  | Mul
  | Quo
  | LParen
  | RParen
  deriving (Show)

data NonTerm =
    Node [NonTerm]
  | Leaf Term
  | Choice [NonTerm]
  | Point NonTerm
  deriving (Show)

(<+>) :: NonTerm -> NonTerm -> NonTerm
Choice x <+> Choice y = Choice $ x ++ y
Choice x <+> y = Choice $ x ++ [y]
x <+> Choice y = Choice $ x : y
x <+> y = Choice [x, y]

(</>) :: NonTerm -> NonTerm -> NonTerm
Node x </> Node y = Node $ x ++ y
Node x </> y = Node $ x ++ [y]
x </> Node y = Node $ x : y
x </> y = Node [x, y]

instance Monoid NonTerm where
  mempty = Node []
  mappend x y = x </> y

terminal :: Term -> NonTerm
terminal = Leaf

expr :: NonTerm
expr = Choice [ Node [ expr, terminal Add, term ], term ]

term :: NonTerm
term = Choice [ Node [ term, terminal Mul, factor ], factor ]

factor :: NonTerm
factor = Choice [ Node [ terminal LParen, expr, terminal RParen ], terminal Num ]

data Pending a =
    Decision a
  | Pending

instance Monad Pending where
  (Decision x) >>= k = k x
  Pending >>= _ = Pending

nullable :: NonTerm -> Pending Bool
nullable (Leaf Eps) = Decision True
nullable (Leaf n) = Decision False
nullable (Choice ts) = any nullable ts
nullable (Node ts) = (flip all) ts <$> nullable
nullable (Point t) = Pending
