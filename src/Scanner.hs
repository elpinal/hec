module Scanner
    ( scan
    , Token(..)
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
  | Mul
  | WhiteSpace
    deriving (Eq, Show, Ord)

scan :: String -> Either ParseError [Token]
scan src = filter isNotWhiteSpace <$> scan' src
  where
    isNotWhiteSpace :: Token -> Bool
    isNotWhiteSpace (Token (WhiteSpace, _)) = False
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
  (curry Token Num <$> many1 digit)
  <|> (curry Token Ident <$> many1 (letter <|> digit))
  <|> (curry Token Str <$> scanString)
  <|> (curry Token Add . (:[]) <$> char '+')
  <|> (curry Token Sub . (:[]) <$> char '-')
  <|> (curry Token Mul . (:[]) <$> char '*')
  <|> (curry Token WhiteSpace <$> many1 space)

scanString :: GenParser Char st String
scanString = do
  char '"'
  s <- many (noneOf "\"")
  char '"'
  return s
