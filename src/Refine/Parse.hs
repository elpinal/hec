module Refine.Parse
  ( parseExpr
  , Expr(..)
  ) where

import Data.Functor

import Text.ParserCombinators.Parsec

data Expr =
    Num Int
  | Bool Bool
  | Succ
  | ToInt
  | App Expr Expr
    deriving (Eq, Show)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (parseApp <* eof) "<no filename>"

parseNum :: Parser Expr
parseNum = Num . read <$> many1 digit
     <|> between (char '(') (char ')') parseApp

parseApp :: Parser Expr
parseApp = App <$> parseSucc <* many1 space <*> parseNum
       <|> App <$> parseBoolF <* many1 space <*> parseBool
       <|> parseNum

parseSucc :: Parser Expr
parseSucc = Succ <$ string "succ"

parseBoolF :: Parser Expr
parseBoolF = ToInt <$ string "toInt"

parseBool :: Parser Expr
parseBool = Bool False <$ string "False"
        <|> Bool True <$ string "True"
