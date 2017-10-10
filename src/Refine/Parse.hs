module Refine.Parse
  ( parseExpr
  , Expr(..)
  ) where

import Text.Parsec
import Text.Parsec.String

data Expr =
    Num Int
  | Bool Bool
  | Succ
  | ToInt
  | BinOp String Expr Expr
  | App Expr Expr
  | Var String
    deriving (Eq, Show)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (parseBinOp parseApp <* eof) "<no filename>"

parseBinOp :: Parser Expr -> Parser Expr
parseBinOp p = do
  x <- p
  try (parseBinOp' x) <|> return x
  where
    parseBinOp' :: Expr -> Parser Expr
    parseBinOp' lhs = do
      many space
      lit <- many1 (oneOf "!#$%&+/<=>?@")
      many space
      rhs <- parseApp
      parseBinOp . return $ BinOp lit lhs rhs

parseNum :: Parser Expr
parseNum = Num . read <$> many1 digit
     <|> between (char '(') (char ')') (parseBinOp parseApp)

parseApp :: Parser Expr
parseApp = App <$> parseSucc <* many1 space <*> parseNum
       <|> App <$> parseBoolF <* many1 space <*> parseBool
       <|> try (App <$> parseIdent <* many1 space <*> parseNum)
       <|> parseNum
       <|> parseIdent

parseIdent :: Parser Expr
parseIdent = do
  x <- lower
  xs <- many $ alphaNum <|> char '\''
  return . Var $ x : xs

parseSucc :: Parser Expr
parseSucc = Succ <$ string "succ"

parseBoolF :: Parser Expr
parseBoolF = ToInt <$ string "toInt"

parseBool :: Parser Expr
parseBool = Bool False <$ string "False"
        <|> Bool True <$ string "True"
