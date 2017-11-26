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
  , varid
  , typeFn
  , typeApp
  , typeTerm
  , labeledType
  , variantType
  , recordR
  ) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token hiding (symbol)
import qualified Text.Parsec.Token as Token

import Refine.AST hiding (bool)
import Refine.Kind
import Refine.Type
import qualified Refine.Type.Syntactic as S

parseWhole :: Parser a -> String -> Either ParseError a
parseWhole p = parse' $ p <* eof

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p "<no filename>"

parseExpr :: String -> Either ParseError Expr
parseExpr = parseWhole expression

surroundedBySpaces :: Parser a -> Parser a
surroundedBySpaces = between (many space) $ many space

symbols :: String
symbols = "!#$%&+/<=>?@:"

symbol :: Parsec String u Char
symbol = oneOf symbols

paren :: Parser a -> Parser a
paren = between (char '(' >> many space) (many space >> char ')')

keyword :: String -> Parser String
keyword s = do
  string s
  notFollowedBy $ alphaNum <|> char '\''
  return s

keywords :: [String]
keywords = ["type", "case", "of", "newtype", "data"]

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
  name <- varid
  arg <- optionMaybe parseArg
  surroundedBySpaces $ char '='
  e <- expression
  return . VarDecl name $ maybe e (flip Abs e) arg

parseArg :: Parser String
parseArg = try varid

parseTypeAnn :: Parser Decl
parseTypeAnn = do
  name <- varid
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
              <|> TypeVar . flip TVar Star <$> varid
              <|> try unitT
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
parsePat = PVar <$> varid
       <|> PWildcard <$ string "_"
       <|> PLit <$> literal
       <|> parsePAs

parsePAs :: Parser Pat
parsePAs =  do
  i <- varid
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
projField = fieldSpecifier >> varid

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
      s <- varid
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
      s <- varid
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

{-- Refined parsers with proper lexers --}

def :: LanguageDef st
def = emptyDef
  { identStart = letter
  , identLetter = alphaNum <|> char '\''
  , opStart = symbol
  , opLetter = symbol
  , reservedNames = keywords
  , reservedOpNames = ["\\", "->", "=", "|", "::"]
  }

lexer :: TokenParser st
lexer = makeTokenParser def

ident :: Parser String
ident = identifier lexer <?> "identifier"

varid :: Parser String
varid = lookAhead lower >> ident

conid :: Parser String
conid = lookAhead upper >> ident

variable :: Parser Expr
variable = Var <$> varid <?> "variable"

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
  operator lexer <|> infixed

infixed :: Parser String
infixed = between (Token.symbol lexer "`") (Token.symbol lexer "`") ident
  <?> "infixed function"

expression :: Parser Expr
expression = lambdaAbs <|> binary

lambdaAbs :: Parser Expr
lambdaAbs = flip label "lambda abstraction" $ do
  reservedOp lexer "\\"
  i <- varid
  rightArrow
  e <- expression
  return $ Abs i e

rightArrow = reservedOp lexer "->"

tuple :: Parser Expr
tuple = flip label "tuple" $ fmap Tuple . parens lexer $ commaSep2 expression

commaSep2 :: Parser a -> Parser [a]
commaSep2 p = sepBy2 p $ comma lexer

unitT :: Parser Type
unitT = tUnit <$ do
  Token.symbol lexer "("
  Token.symbol lexer ")"

{-- Refined type parsers (for S.Type) --}

typeFn :: Parser S.Type
typeFn = typeApp `chainr1` do
  rightArrow
  return S.fn

typeApp :: Parser S.Type
typeApp = foldl1 S.TypeApp <$> many1 typeTerm

typeTerm :: Parser S.Type
typeTerm = try typeAtom <|> parens lexer typeFn

typeAtom :: Parser S.Type
typeAtom = choice
  [ typeVariable
  , typeCon
  , try unitType
  , tupleType
  ]

typeVariable :: Parser S.Type
typeVariable = S.TypeVar <$> varid

typeCon :: Parser S.Type
typeCon = S.TypeCon <$> conid

unitType :: Parser S.Type
unitType = S.tUnit <$ do
  Token.symbol lexer "("
  Token.symbol lexer ")"

tupleType :: Parser S.Type
tupleType = do
  types <- parens lexer $ commaSep2 typeFn
  let c = S.tTupleN $ length types
  return $ foldl S.TypeApp c types

varDecl :: Parser (String, Expr)
varDecl = do
  i <- varid
  equal
  e <- expression
  return (i, e)

typeAnn :: Parser (String, S.Type)
typeAnn = do
  i <- varid
  reservedOp lexer "::"
  t <- typeFn
  return (i, t)

typeSynonym :: Parser (String, S.Type)
typeSynonym = do
  reserved lexer "type"
  i <- conid
  equal
  t <- typeFn
  return (i, t)

typeDecl :: Parser (String, [(String, [S.Type])])
typeDecl = do
  reserved lexer "data"
  i <- conid
  equal
  t <- variantType
  return (i, t)

variantType :: Parser [(String, [S.Type])]
variantType = labeledType `sepBy1` reservedOp lexer "|"

labeledType :: Parser (String, [S.Type])
labeledType = (,) <$> conid <*> (many1 typeTerm)

equal :: Parser ()
equal = reservedOp lexer "="

recordR :: Parser [(String, Expr)]
recordR = braces lexer $ commaSep lexer varDecl

recordTypeR :: Parser [(String, S.Type)]
recordTypeR = braces lexer $ commaSep lexer typeSynonym
