{-# LANGUAGE GADTs #-}

module Refine.Parse
  (
  ) where

import Text.ParserCombinators.Parsec

data Expr a where
  Const :: a -> Expr a
  Func :: Expr a -> Expr a

parseExpr :: Read a => String -> Either ParseError (Expr a)
parseExpr = parse parser "<no filename>"

parser :: Read a => Parser (Expr a)
parser = Const . read <$> many1 digit
