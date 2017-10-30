module Refine.Parse
  ( parseExpr
  , Expr(..)
  , Literal(..)

  , parseWhole
  , parse'
  , parseDecl
  , parseType
  , parseType'
  , parseTypeSig
  , parseTypeDecl
  , parseList'
  , parseEmptyList
  , parseCase
  , parseIdent'
  , Decl(..)
  , keyword
  ) where

import Text.Parsec
import Text.Parsec.String

import Refine.AST
import Refine.Type

parseWhole :: Parser a -> String -> Either ParseError a
parseWhole p = parse' $ p <* eof

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p "<no filename>"

parseExpr :: String -> Either ParseError Expr
parseExpr = parseWhole parseExpr'

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
symbol = oneOf "!#$%&+/<=>?@:"

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

keyword :: String -> Parser String
keyword s = do
  string s
  notFollowedBy $ alphaNum <|> char '\''
  return s

keywords :: [String]
keywords = ["type", "case", "of"]

parseIdent' :: Parser String
parseIdent' = do
  x <- lower
  xs <- many $ alphaNum <|> char '\''
  if (x : xs) `elem` keywords
    then unexpected $ "keyword: " ++ show (x : xs)
    else return $ x : xs

parseLit :: Parser Expr
parseLit = Lit <$> parseLit'

parseLit' :: Parser Literal
parseLit' = parseNum
        <|> parseBool
        <|> parseChar
        <|> parseString
        <|> parseEmptyList

parseNum :: Parser Literal
parseNum = LitInt . read <$> many1 digit

parseBool :: Parser Literal
parseBool = LitBool False <$ string "False"
        <|> LitBool True <$ string "True"

parseChar :: Parser Literal
parseChar = fmap LitChar $ between (char '\'') (char '\'') $ escapedChar <|> noneOf "'"

escapedChar :: Parser Char
escapedChar = char '\\' >> char '\''

parseString :: Parser Literal
parseString = fmap LitString $ between (char '"') (char '"') . many $ escapedString <|> noneOf "\""

escapedString :: Parser Char
escapedString = char '\\' >> char '"'

data Decl =
    Decl String Expr
  | TypeSig String Type
  | TypeDecl String Type
  deriving (Eq, Show)

parseDecl :: Parser Decl
parseDecl = do
  name <- parseIdent'
  arg <- optionMaybe parseArg
  surroundedBySpaces $ char '='
  e <- parseExpr'
  return . Decl name $ maybe e (flip Abs e) arg

parseArg :: Parser String
parseArg = try $ many1 space >> parseIdent'

parseTypeSig :: Parser Decl
parseTypeSig = do
  name <- parseIdent'
  surroundedBySpaces $ string "::"
  t <- parseType
  return $ TypeSig name t

parseType :: Parser Type
parseType =
  chainr1 (readType <$> parseTypeIdent) $ do
    many space
    string "->"
    many space
    return fn

parseType' :: Parser Type
parseType' = try parseFunctionType <|> parseTypeTerm

parseSimpleType :: Parser Type
parseSimpleType = readType <$> parseTypeIdent
              <|> paren parseSimpleType

parseTypeTerm :: Parser Type
parseTypeTerm = paren (try parseFunctionType <|> parseSimpleType)
            <|> parseSimpleType

parseFunctionType :: Parser Type
parseFunctionType = do
  t <- parseTypeTerm
  many space
  string "->"
  many space
  fmap (fn t) $ try parseFunctionType <|> parseTypeTerm

parseTypeIdent :: Parser String
parseTypeIdent = do
  x <- letter
  xs <- many $ alphaNum <|> char '\''
  return $ x : xs

readType :: String -> Type
readType "Int" = tInt
readType "Bool" = tBool
readType "Char" = tChar
readType "String" = tString

parseTypeDecl :: Parser Decl
parseTypeDecl = do
  keyword "type"
  many1 space
  s <- parseTypeIdent
  many space
  char '='
  many space
  t <- parseType
  return $ TypeDecl s t

parsePat :: Parser Pat
parsePat = PVar <$> parseIdent'
       <|> PWildcard <$ string "_"
       <|> PLit <$> parseLit'

parseCase :: Parser Expr
parseCase = do
  keyword "case"
  many space
  e <- parseExpr'
  many space
  keyword "of"
  many space
  p <- parsePat
  many space
  string "->"
  many space
  e1 <- parseExpr'
  return $ Case e [(p, e1)]

parseList' :: Parser [Expr]
parseList' = do
  char '['
  es <- surroundedBySpaces parseExpr' `sepBy` string ","
  char ']'
  return es

parseEmptyList :: Parser Literal
parseEmptyList = do
  char '['
  many space
  char ']'
  return LitEmptyList
