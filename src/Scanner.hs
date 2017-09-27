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

type Token = Token1 Term

createToken :: String -> Term -> Token
createToken = Token1

getTerm :: Token -> Term
getTerm (Token1 _ term) = term

fromNum :: (Read a, Num a) => Token -> a
fromNum (Token1 val Num) = read val
fromNum t = error $ "not a number: " ++ show t

scan :: String -> Either ParseError [Token]
scan src = filter isNotWhiteSpace <$> scan' src
  where
    isNotWhiteSpace :: Token -> Bool
    isNotWhiteSpace = (/= WhiteSpace) . getTerm

scan' :: String -> Either ParseError [Token]
scan' = parse lexeme "<unknown>"

lexeme :: Parser [Token]
lexeme = do
  expr <- many scanExpr
  eof
  return expr

scanExpr :: Parser Token
scanExpr =
      Token1         <$> many1 digit              <*> return Num
  <|> Token1         <$> many1 (letter <|> digit) <*> return Ident
  <|> Token1         <$> scanString               <*> return Str
  <|> Token1 . (:[]) <$> char '+'                 <*> return Add
  <|> Token1 . (:[]) <$> char '-'                 <*> return Sub
  <|> Token1 . (:[]) <$> char '*'                 <*> return Mul
  <|> Token1         <$> many1 space              <*> return WhiteSpace

scanString :: Parser String
scanString = do
  char '"'
  s <- many $ noneOf "\""
  char '"'
  return s
