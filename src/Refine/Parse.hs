module Refine.Parse
  ( parseExpr
  , Expr(..)
  , Literal(..)

  , parseWhole
  , parse'
  , parseDecl
  , parseVarDecl
  , parseTypeIdent
  , parseType
  , parseType'
  , parseTupleType
  , parseTypeAnn
  , parseTypeDecl
  , parseList'
  , parseEmptyList
  , parseCase
  , parseIdent
  , parseNewType
  , parseTuple
  , parsePAs
  , Decl(..)
  , keyword
  , record
  , recordType
  , dataDecl
  , infixed
  , ident
  ) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token hiding (symbol)
import qualified Text.Parsec.Token as Token

import Refine.AST hiding (bool)
import Refine.Kind
import Refine.Type

parseWhole :: Parser a -> String -> Either ParseError a
parseWhole p = parse' $ p <* eof

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p "<no filename>"

parseExpr :: String -> Either ParseError Expr
parseExpr = parseWhole expression

parseAbs :: Parser Expr
parseAbs = lambdaAbs

parseBinOp :: Parser Expr
parseBinOp = binary

surroundedBySpaces :: Parser a -> Parser a
surroundedBySpaces = between (many space) $ many space

symbols :: String
symbols = "!#$%&+/<=>?@:"

symbol :: Parsec String u Char
symbol = oneOf symbols

parseApp :: Parser Expr
parseApp = app

parseTerm :: Parser Expr
parseTerm = term

paren :: Parser a -> Parser a
paren = between (char '(' >> many space) (many space >> char ')')

parseVar :: Parser Expr
parseVar = variable

keyword :: String -> Parser String
keyword s = do
  string s
  notFollowedBy $ alphaNum <|> char '\''
  return s

keywords :: [String]
keywords = ["type", "case", "of", "newtype", "data"]

parseIdent :: Parser String
parseIdent = ident

parseLit :: Parser Expr
parseLit = Lit <$> literal

parseLit' :: Parser Literal
parseLit' = literal

parseUnit :: Parser Literal
parseUnit = unit

parseNum :: Parser Literal
parseNum = number

parseBool :: Parser Literal
parseBool = bool

parseChar :: Parser Literal
parseChar = character

parseString :: Parser Literal
parseString = str

data Decl =
    VarDecl String Expr
  | TypeAnn String Type
  | TypeDecl String Type
  | NewTypeDecl String String Type
  | DataDecl String [(String, [Type])]
  deriving (Eq, Show)

parseDecls :: Parser [Decl]
parseDecls = parseDecl `sepBy` newline

parseDecl :: Parser Decl
parseDecl = try parseVarDecl <|> try parseTypeAnn <|> parseTypeDecl

parseVarDecl :: Parser Decl
parseVarDecl = do
  name <- ident
  arg <- optionMaybe parseArg
  surroundedBySpaces $ char '='
  e <- expression
  return . VarDecl name $ maybe e (flip Abs e) arg

parseArg :: Parser String
parseArg = try ident

parseTypeAnn :: Parser Decl
parseTypeAnn = do
  name <- ident
  surroundedBySpaces $ string "::"
  t <- parseType'
  return $ TypeAnn name t

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
              <|> TypeVar . flip TVar Star <$> ident
              <|> try unitType
              <|> try parseTupleType
              <|> paren parseSimpleType
              <|> recordType

parseTypeTerm :: Parser Type
parseTypeTerm = try parseSimpleType
            <|> paren (try parseFunctionType <|> try parseSimpleType)

parseTupleType :: Parser Type
parseTupleType = do
  char '('
  many space
  ts <- (parseType' <* many space) `sepBy2` try (char ',' >> many space)
  char ')'
  return $ foldl TypeApp (tTupleN $ length ts) ts

parseFunctionType :: Parser Type
parseFunctionType = do
  t <- parseTypeTerm
  many space
  string "->"
  many space
  fmap (fn t) $ try parseFunctionType <|> parseTypeTerm

parseTypeIdent :: Parser String
parseTypeIdent = do
  x <- upper
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
parsePat = PVar <$> ident
       <|> PWildcard <$ string "_"
       <|> PLit <$> parseLit'
       <|> parsePAs

parsePAs :: Parser Pat
parsePAs =  do
  i <- ident
  many space
  char '@'
  many space
  p <- parsePat
  return $ PAs i p

parseCase :: Parser Expr
parseCase = do
  keyword "case"
  many space
  e <- expression
  many space
  keyword "of"
  many space
  p <- parsePat
  many space
  string "->"
  many space
  e1 <- expression
  return $ Case e [(p, e1)]

parseList' :: Parser [Expr]
parseList' = do
  char '['
  es <- surroundedBySpaces expression `sepBy` string ","
  char ']'
  return es

parseEmptyList :: Parser Literal
parseEmptyList = do
  char '['
  many space
  char ']'
  return LitEmptyList

parseNewType :: Parser Decl
parseNewType = do
  keyword "newtype"
  many space
  s <- parseTypeIdent
  many space
  char '='
  many space
  con <- parseTypeIdent
  many space
  t <- parseTypeTerm
  return $ NewTypeDecl s con t

parseTuple :: Parser Expr
parseTuple = do
  char '('
  many space
  es <- (expression <* many space) `sepBy2` try (char ',' >> many space)
  char ')'
  return $ Tuple es

sepBy2 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy2 p sep = do
  x <- p
  xs <- many1 $ sep >> p
  return $ x : xs

fieldSpecifier :: Parser String
fieldSpecifier = string "\\/"

projField :: Parser String
projField = fieldSpecifier >> ident

record :: Parser Expr
record = do
  char '{'
  many space
  xs <- (f <* many space) `sepBy` (char ',' >> many space)
  char '}'
  return $ Record xs
  where
    f :: Parser (String, Expr)
    f = do
      s <- ident
      many space
      char '='
      many space
      e <- expression
      return (s, e)

recordType :: Parser Type
recordType = do
  char '{'
  many space
  xs <- (f <* many space) `sepBy` (char ',' >> many space)
  char '}'
  return $ foldl TypeApp (tRecordN $ map fst xs) $ map snd xs
  where
    f :: Parser (String, Type)
    f = do
      s <- ident
      many space
      char '='
      many space
      t <- parseType'
      return (s, t)

dataDecl :: Parser Decl
dataDecl = do
  keyword "data"
  many space
  t <- parseTypeIdent
  many space
  char '='
  xs <- f `sepBy1` (many space >> char '|')
  return $ DataDecl t xs
  where
    f :: Parser (String, [Type])
    f = do
      many space
      c <- parseTypeIdent
      many space
      ts <- parseTypeTerm `sepBy` many space
      return (c, ts)

unitType :: Parser Type
unitType = tUnit <$ (char '(' >> many space >> char ')')

infixed :: Parser String
infixed = between (char '`') (char '`') $ ident

def :: LanguageDef st
def = emptyDef
  { identStart = lower
  , identLetter = alphaNum <|> char '\''
  , opStart = symbol
  , opLetter = symbol
  , reservedNames = keywords
  , reservedOpNames = ["\\", "->"]
  }

lexer :: TokenParser st
lexer = makeTokenParser def

ident :: Parser String
ident = identifier lexer <?> "identifier"

variable :: Parser Expr
variable = Var <$> ident <?> "variable"

number :: Parser Literal
number = LitInt . fromIntegral <$> (lexeme <*> decimal) lexer <?> "number"

bool :: Parser Literal
bool = LitBool False <$ lexeme lexer (string "False")
   <|> LitBool True  <$ lexeme lexer (string "True")
  <?> "bool"

character :: Parser Literal
character = LitChar <$> charLiteral lexer <?> "character"

str :: Parser Literal
str = LitString <$> stringLiteral lexer <?> "string"

emptyList :: Parser Literal
emptyList = LitEmptyList <$ (Token.symbol lexer "[" >> Token.symbol lexer "]") <?> "[]"

unit :: Parser Literal
unit = LitUnit <$ (Token.symbol lexer "(" >> Token.symbol lexer ")") <?> "unit"

literal :: Parser Literal
literal = number
      <|> bool
      <|> character
      <|> str
      <|> emptyList
      <|> unit
  <?> "literal"

term :: Parser Expr
term = choice
  [ try $ Lit <$> literal
  , variable
  , try tuple
  , parens lexer expression
  ] <?> "term"

app :: Parser Expr
app = term `chainl1` return App <?> "function application"

binary :: Parser Expr
binary = flip label "binary operation" $ do
  as <- chainl1 app . try $ operate <* notFollowedBy lambdaAbs
  option as $ do
    o <- operate
    a <- lambdaAbs
    return $ o as a

operate :: Parser (Expr -> Expr -> Expr)
operate = flip label "binary operator" $ fmap BinOp $
  operator lexer <|> infixed1

infixed1 :: Parser String
infixed1 = between (Token.symbol lexer "`") (Token.symbol lexer "`") ident
  <?> "infixed function"

expression :: Parser Expr
expression = lambdaAbs <|> binary

lambdaAbs :: Parser Expr
lambdaAbs = flip label "lambda abstraction" $ do
  reservedOp lexer "\\"
  i <- ident
  reservedOp lexer "->"
  e <- expression
  return $ Abs i e

tuple :: Parser Expr
tuple = flip label "tuple" $ fmap Tuple . parens lexer $ commaSep2 expression

commaSep2 :: Parser a -> Parser [a]
commaSep2 p = sepBy2 p $ comma lexer
