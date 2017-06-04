module Scan
    ( Token(..)
    , Term(..)
    ) where

import Text.ParserCombinators.Parsec

---------- Tokenizer ----------

newtype Token = Token (Term, String)
  deriving (Eq, Show, Ord)

data Term =
    Num
  | Ident
  | Str
  | Add
  | Sub
  | WhiteSpace
    deriving (Eq, Show, Ord)

scan :: String -> Either ParseError [Token]
scan src =
  case scan' src of
    Right ts -> Right $ filter isNotWhiteSpace ts
    x -> x
  where isNotWhiteSpace (Token (WhiteSpace, _)) = False
        isNotWhiteSpace _ = True

scan' :: String -> Either ParseError [Token]
scan' = parse lexeme "<unknown>"

lexeme :: GenParser Char st [Token]
lexeme = do
  expr <- many scanExpr
  eof
  return expr

scanExpr :: GenParser Char st Token
scanExpr =
  (many1 digit >>= return . curry Token Num)
  <|> (many1 (letter <|> digit) >>= return . curry Token Ident)
  <|> (scanString >>= return . curry Token Str)
  <|> (char '+' >>= return . curry Token Add . (:[]))
  <|> (char '-' >>= return . curry Token Sub . (:[]))
  <|> (many1 space >>= return . curry Token WhiteSpace)

scanString :: GenParser Char st String
scanString = do
  char '"'
  s <- many (noneOf "\"")
  char '"'
  return s
