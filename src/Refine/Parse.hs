module Refine.Parse
  ( parseExpr
  , Decl(..)
  , Expr(..)
  , Literal(..)

  , parseWhole
  , parse'
  , decls
  , decl
  , varDecl
  , parseTypeAnn
  , parseTypeDecl
  , parseList'
  , parseCase
  , parsePat
  , parsePAs
  , dataDecl
  , infixed
  , ident
  , varid
  , typeFn
  , typeApp
  , typeTerm
  , labeledType
  , variantType
  , tuple
  , record
  , recordTypeR
  , emptyList
  ) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Refine.AST hiding (bool)
import Refine.Type.Syntactic

parseWhole :: Parser a -> String -> Either ParseError a
parseWhole p = parse' $ p <* eof

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p "<no filename>"

parseExpr :: String -> Either ParseError Expr
parseExpr = parseWhole expression

decls :: Parser [Decl]
decls = decl `sepEndBy` semi lexer

decl :: Parser Decl
decl = choice
  [ uncurry VarDecl <$> try varDecl
  , parseTypeAnn
  , parseTypeDecl
  , dataDecl
  ]

parseTypeAnn :: Parser Decl
parseTypeAnn = uncurry TypeAnn <$> typeAnn

parseTypeDecl :: Parser Decl
parseTypeDecl = uncurry TypeDecl <$> typeSynonym

parsePat :: Parser Pat
parsePat = choice
  [ try parsePAs
  , PVar <$> try varid
  , PWildcard <$ reserved lexer "_"
  , PLit <$> try literal
  , conPat
  , tuplePat
  ]

conPat :: Parser Pat
conPat = PCon <$> conid <*> many parsePat

parsePAs :: Parser Pat
parsePAs =  do
  i <- varid
  reservedOp lexer "@"
  p <- parsePat
  return $ PAs i p

tuplePat :: Parser Pat
tuplePat = do
  ps <- parens lexer $ commaSep2 parsePat
  return $ PCon ("(," ++ show (length ps) ++ ")") ps

parseCase :: Parser Expr
parseCase = do
  reserved lexer "case"
  e <- expression
  reserved lexer "of"
  p <- parsePat
  rightArrow
  e1 <- expression
  -- TODO: Support multiple branches.
  return $ Case e [(p, e1)]

parseList' :: Parser [Expr]
parseList' = brackets lexer $ commaSep lexer expression

fieldSpecifier :: Parser ()
fieldSpecifier = reservedOp lexer "\\/"

projField :: Parser String
projField = fieldSpecifier >> varid

dataDecl :: Parser Decl
dataDecl = uncurry DataDecl <$> typeDecl

{-- Refined parsers with proper lexers --}

symbols :: String
symbols = "!#$%&+/<=>?@:"

keywords :: [String]
keywords = ["type", "case", "of", "data", "_"]

def :: LanguageDef st
def = emptyDef
  { identStart = letter
  , identLetter = alphaNum <|> char '\''
  , opStart = symbol
  , opLetter = symbol
  , reservedNames = keywords
  , reservedOpNames = ["\\", "->", "=", "|", "::", ",", "\\/", "@"]
  }
  where
    symbol :: Parsec String u Char
    symbol = oneOf symbols

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
emptyList = LitEmptyList <$ (brackets lexer $ return ()) <?> "[]"

unit :: Parser Literal
unit = LitUnit <$ (parens lexer $ return ()) <?> "unit"

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
  , record
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
infixed = between (symbol lexer "`") (symbol lexer "`") ident
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

sepBy2 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy2 p sep = do
  x <- p
  xs <- many1 $ sep >> p
  return $ x : xs

commaSep2 :: Parser a -> Parser [a]
commaSep2 p = sepBy2 p $ comma lexer

{-- Refined type parsers (for Type) --}

typeFn :: Parser Type
typeFn = typeApp `chainr1` do
  rightArrow
  return fn

typeApp :: Parser Type
typeApp = foldl1 TypeApp <$> many1 typeTerm

typeTerm :: Parser Type
typeTerm = try typeAtom <|> parens lexer typeFn

typeAtom :: Parser Type
typeAtom = choice
  [ typeVariable
  , typeCon
  , try unitType
  , tupleType
  , recordTypeR
  ]

typeVariable :: Parser Type
typeVariable = TypeVar <$> varid

typeCon :: Parser Type
typeCon = TypeCon <$> conid

unitType :: Parser Type
unitType = tUnit <$ do
  symbol lexer "("
  symbol lexer ")"

tupleType :: Parser Type
tupleType = do
  types <- parens lexer $ commaSep2 typeFn
  let c = tTupleN $ length types
  return $ foldl TypeApp c types

varDecl :: Parser (String, Expr)
varDecl = eq varid expression

typeAnn :: Parser (String, Type)
typeAnn = do
  i <- varid
  reservedOp lexer "::"
  t <- typeFn
  return (i, t)

typeSynonym :: Parser (String, Type)
typeSynonym = do
  reserved lexer "type"
  eq conid typeFn

typeDecl :: Parser (String, Type)
typeDecl = do
  reserved lexer "data"
  eq conid variantType

variantType :: Parser Type
variantType = do
  fs <- labeledType `sepBy1` reservedOp lexer "|"
  let t = tVariant $ map fst fs
      ts = map snd fs
  return $ foldl TypeApp t ts

labeledType :: Parser (String, Type)
labeledType = do
  i <- conid
  ts <- many typeTerm
  return (i, foldl TypeApp (tTupleN $ length ts) ts)

equal :: Parser ()
equal = reservedOp lexer "="

eq :: Parser a -> Parser b -> Parser (a, b)
eq p q = do
  x <- p
  equal
  y <- q
  return (x, y)

record :: Parser Expr
record = fmap Record . braces lexer $ commaSep lexer varDecl

recordTypeR :: Parser Type
recordTypeR = do
  r <- p
  let t = tRecordN $ map fst r
      ts = map snd r
  return $ foldl TypeApp t ts
  where
    p :: Parser [(String, Type)]
    p = flip label "record type" $ braces lexer . commaSep lexer $ eq varid typeFn
