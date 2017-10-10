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
    deriving (Eq, Show)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (parseBinOp <* eof) "<no filename>"

parseBinOp :: Parser Expr
parseBinOp = do
  x <- parseApp
  try (parseBinOp' <*> return x) <|> return x
  where
    parseBinOp' :: Parser (Expr -> Expr)
    parseBinOp' = do
      many space
      lit <- many1 (oneOf "!#$%&+/<=>?@")
      many space
      rhs <- parseApp
      return $ \lhs -> BinOp lit lhs rhs

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
