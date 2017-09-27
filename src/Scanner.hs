module Scanner
    ( scan
    , scanWithFilename
    , Token
    , Token1(..) -- exported for testing
    , Term(..)
    , createToken
    , getTerm
    , fromNum
    , scanString
    ) where

import Control.Monad

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

scanWithFilename :: FilePath -> String -> Either ParseError [Token]
scanWithFilename = (return . filter isNotWhiteSpace <=<) . parse lexeme
  where
    isNotWhiteSpace :: Token -> Bool
    isNotWhiteSpace = (/= WhiteSpace) . getTerm

scan :: String -> Either ParseError [Token]
scan = scanWithFilename "<input from string>"

lexeme :: Parser [Token]
lexeme = do
  expr <- many scanExpr
  eof
  return expr

infix 2 $$
($$) :: (a -> b) -> a -> b
($$) = ($)

scanExpr :: Parser Token
scanExpr =
      tokenize Num        $$ many1 digit
  <|> tokenize Ident      $$ many1 (letter <|> digit)
  <|> tokenize Str        $$ scanString
  <|> tokenize Add        $$ string "+"
  <|> tokenize Sub        $$ string "-"
  <|> tokenize Mul        $$ string "*"
  <|> tokenize WhiteSpace $$ many1 space

tokenize :: Term -> Parser String -> Parser Token
tokenize t p = Token1 <$> p <*> return t

scanString :: Parser String
scanString = do
  char '"'
  s <- many $ noneOf "\""
  char '"'
  return s
