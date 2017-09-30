{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Refine.Parse
  ( parseExpr
  , parser
  , eval
  , Expr(..)
  ) where

import Data.Functor

import Text.ParserCombinators.Parsec

data Expr a where
  Num :: (Num a, Read a) => a -> Expr a
  Bool :: Bool -> Expr Bool
  Succ :: (Num a, Read a) => Expr a -> Expr a
  ToInt :: (Num a, Read a) => Expr Bool -> Expr a

deriving instance Eq a => Eq (Expr a)

deriving instance Show a => Show (Expr a)

eval :: Expr a -> a
eval (Num n) = n
eval (Bool b) = b
eval (Succ e) = eval e + 1
eval (ToInt e) = if eval e then 1 else 0

parseExpr :: (Num a, Read a) => String -> Either ParseError (Expr a)
parseExpr = parse (parser <* eof) "<no filename>"

parser :: (Num a, Read a) => Parser (Expr a)
parser = Num . read <$> many1 digit
     <|> parseApp <*> parser
     <|> parseBoolF <*> parseBool

parseApp :: (Num a, Read a) => Parser (Expr a -> Expr a)
parseApp = Succ <$ string "succ"

parseBoolF :: (Num a, Read a) => Parser (Expr Bool -> Expr a)
parseBoolF = ToInt <$ string "toInt"

parseBool :: Parser (Expr Bool)
parseBool = Bool False <$ string "False"
        <|> Bool True <$ string "True"
