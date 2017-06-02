module Scan
    ( Token
    , Term
    ) where

---------- Tokenizer ----------

newtype Token = Token (Term, String)

data Term =
    Num
  | Ident
  | Str
  | BinOp
    deriving (Eq, Show)

------

scan :: String -> Either ParseError [Token]
scan src =
  case scan' src of
    Right ts -> Right $ filter (/= WhiteSpace) ts
    x -> x

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
  <|> (oneOf "+-" >>= return . curry Token BinOp . (:[]))
  <|> (many1 space >>= return . curry Token WhiteSpace)

scanString :: GenParser Char st String
scanString = do
  char '"'
  s <- many (noneOf "\"")
  char '"'
  return s
