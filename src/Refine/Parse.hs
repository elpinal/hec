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
  | LitChar Char
  | LitString String
  deriving (Eq, Show)

data Expr =
    Lit Literal
  | BinOp String Expr Expr
  | App Expr Expr
  | Var String
  | Abs String Expr
    deriving (Eq, Show)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (parseExpr' <* eof) "<no filename>"

parseExpr' :: Parser Expr
parseExpr' = parseAbs <|> parseBinOp

parseAbs :: Parser Expr
parseAbs = do
  char '\\'
  many space
  s <- parseIdent'
  many space
  string "->"
  many space
  body <- parseExpr'
  return $ Abs s body

parseBinOp :: Parser Expr
parseBinOp = followTo parseApp
  where
    followTo :: Parser Expr -> Parser Expr
    followTo p = do
      x <- p
      try (parseBinOp' x) <|> return x

    parseBinOp' :: Expr -> Parser Expr
    parseBinOp' lhs = do
      op <- BinOp <$> (surroundedBySpaces . many1) symbol <*> return lhs
      fmap op parseAbs <|> followTo (fmap op parseApp)

surroundedBySpaces :: Parser a -> Parser a
surroundedBySpaces = between (many space) $ many space

symbol :: Parser Char
symbol = oneOf "!#$%&+/<=>?@"

parseApp :: Parser Expr
parseApp = followTo parseTerm
  where
    followTo :: Parser Expr -> Parser Expr
    followTo p = do
      x <- p
      try (followTo $ flip App <$> (many1 space *> parseTerm) <*> return x) <|> return x

parseTerm :: Parser Expr
parseTerm = parseLit
        <|> parseIdent
        <|> paren parseExpr'

paren :: Parser a -> Parser a
paren = between (char '(' >> many space) (many space >> char ')')

parseIdent :: Parser Expr
parseIdent = Var <$> parseIdent'

parseIdent' :: Parser String
parseIdent' = do
  x <- lower
  xs <- many $ alphaNum <|> char '\''
  return $ x : xs

parseLit :: Parser Expr
parseLit = fmap Lit $ parseNum <|> parseBool <|> parseChar <|> parseString

parseNum :: Parser Literal
parseNum = LitInt . read <$> many1 digit

parseBool :: Parser Literal
parseBool = LitBool False <$ string "False"
        <|> LitBool True <$ string "True"

parseChar :: Parser Literal
parseChar = LitChar <$> between (char '\'') (char '\'') (escapedChar <|> noneOf "'")

escapedChar :: Parser Char
escapedChar = char '\\' >> char '\''

parseString :: Parser Literal
parseString = LitString <$> between (char '"') (char '"') (many $ escapedString <|> noneOf "\"")

escapedString :: Parser Char
escapedString = char '\\' >> char '"'

data Decl = Decl String Expr

parseDecl :: Parser Decl
parseDecl = do
  name <- parseIdent'
  char '='
  e <- parseExpr'
  return $ Decl name e
