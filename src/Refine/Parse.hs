{-# LANGUAGE GADTs #-}

module Refine.Parse
  ( parseExpr
  , parser
  , Expr(..)
  ) where

import Text.ParserCombinators.Parsec

data Expr a where
  Const :: a -> Expr a
  Func :: Expr a -> Expr a
    deriving (Eq, Show)

parseExpr :: Read a => String -> Either ParseError (Expr a)
parseExpr = parse parser "<no filename>"

parser :: Read a => Parser (Expr a)
parser = Const . read <$> many1 digit
