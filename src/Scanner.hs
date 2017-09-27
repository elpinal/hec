module Scanner
    ( scan
    , Token
    , Token1(..) -- exported for testing
    , Term(..)
    , createToken
    , getTerm
    , fromNum
    ) where

import Text.ParserCombinators.Parsec

---------- Tokenizer ----------

data Term =
    Num
  | Ident
  | Str
  | Add
  | Sub
  | Mul
  | WhiteSpace
    deriving (Eq, Show, Ord)

data Token1 a = Token1 String a
  deriving (Eq, Show, Ord)

instance Functor Token1 where
  fmap f (Token1 s t) = Token1 s $ f t

newtype Token = Token (Token1 Term)
  deriving (Eq, Show, Ord)

createToken :: String -> Term -> Token
createToken lit t = Token (Token1 lit t)

getTerm :: Token -> Term
getTerm (Token (Token1 _ term)) = term

fromNum :: (Read a, Num a) => Token -> a
fromNum (Token (Token1 val Num)) = read $ val
fromNum t = error $ "not a number: " ++ show t

scan :: String -> Either ParseError [Token]
scan src = filter isNotWhiteSpace <$> scan' src
  where
    isNotWhiteSpace :: Token -> Bool
    isNotWhiteSpace (Token (Token1 _ WhiteSpace)) = False
    isNotWhiteSpace _ = True

scan' :: String -> Either ParseError [Token]
scan' = parse lexeme "<unknown>"

lexeme :: GenParser Char st [Token]
lexeme = do
  expr <- many scanExpr
  eof
  return expr

scanExpr :: GenParser Char st Token
scanExpr = fmap Token $
      (Token1 <$> many1 digit) <*> return Num
  <|> (Token1 <$> many1 (letter <|> digit)) <*> return Ident
  <|> (Token1 <$> scanString) <*> return Str
  <|> (Token1 . (:[]) <$> char '+') <*> return Add
  <|> (Token1 . (:[]) <$> char '-') <*> return Sub
  <|> (Token1 . (:[]) <$> char '*') <*> return Mul
  <|> (Token1 <$> many1 space) <*> return WhiteSpace

scanString :: GenParser Char st String
scanString = do
  char '"'
  s <- many $ noneOf "\""
  char '"'
  return s
