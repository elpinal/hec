module Refine.Parse
  ( parseExpr
  , Expr(..)
  , Literal(..)
  ) where

import Text.Parsec
import Text.Parsec.String

data Literal =
    LitInt Int
  | LitBool Bool
  deriving (Eq, Show)

data Expr =
    Lit Literal
  | BinOp String Expr Expr
  | App Expr Expr
  | Var String
    deriving (Eq, Show)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (parseBinOp (parseApp parseTerm) <* eof) "<no filename>"

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
      rhs <- parseApp parseTerm
      parseBinOp . return $ BinOp lit lhs rhs

parseApp :: Parser Expr -> Parser Expr
parseApp p = do
  x <- p
  try (parseApp' x) <|> return x
  where
    parseApp' :: Expr -> Parser Expr
    parseApp' f = do
      many1 space
      t <- parseTerm
      parseApp . return $ App f t

parseTerm :: Parser Expr
parseTerm = parseLit
        <|> parseIdent
        <|> between (char '(') (char ')') (parseBinOp $ parseApp parseTerm)

parseIdent :: Parser Expr
parseIdent = do
  x <- lower
  xs <- many $ alphaNum <|> char '\''
  return . Var $ x : xs

parseLit :: Parser Expr
parseLit = fmap Lit $ parseNum <|> parseBool

parseNum :: Parser Literal
parseNum = LitInt . read <$> many1 digit

parseBool :: Parser Literal
parseBool = LitBool False <$ string "False"
        <|> LitBool True <$ string "True"
