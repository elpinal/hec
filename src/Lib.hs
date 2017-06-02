module Lib
    ( compile
    ) where

import Data.Char
import Control.Monad

import Text.ParserCombinators.Parsec

compile :: IO ()
compile = interact compile'

compile' :: String -> String
compile' s = case scan s of
  Right tokens -> generate . parse' $ tokens
  Left err -> error . show $ err




---------- Tokenizer ----------

data Token =
    Number Int
  | Ident String
  | Str String
  | BinOp String
  | WhiteSpace
    deriving (Show, Eq)

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
  (many1 digit >>= return . Number . read)
  <|> (many1 (letter <|> digit) >>= return . Ident)
  <|> (scanString >>= return . Str)
  <|> (oneOf "+-" >>= return . BinOp . (:[]))
  <|> (many1 space >> return WhiteSpace)

scanString :: GenParser Char st String
scanString = do
  char '"'
  s <- many (noneOf "\"")
  char '"'
  return s




---------- Parser ----------

data Expr =
    Basic Token
  | Bin Expr Token Expr
  | BadExpr
    deriving (Show, Eq)

parse' :: [Token] -> Expr
parse' = head . foldl f []
  where
    f :: [Expr] -> Token -> [Expr]
    f (x:stack) t@(BinOp o) = Bin x t BadExpr : stack
    f (x:stack) t = case x of
      Bin l o BadExpr -> Bin l o (Basic t) : stack
      _ -> Basic t : x : stack
    f [] (BinOp o) = error $ "unexpected binary operator: " ++ o
    f [] x = [Basic x]




---------- LR(1) Parser ----------

data Symbol =
    Term (Char -> Bool)
  | NonTerm (Char -> Bool)
  | Eps

instance Eq Symbol where
  Eps == Eps = True
  _ == _ = False

(<+>) :: Symbol -> Symbol -> Symbol
(Term x) <+> (Term y) = NonTerm $ \c -> x c || y c
(NonTerm x) <+> (NonTerm y) = NonTerm $ \c -> x c || y c
Eps <+> Eps = Eps

(</>) :: Symbol -> Symbol -> Symbol
(Term x) </> (Term y) = NonTerm $ \c -> x c || y c
(NonTerm x) </> (NonTerm y) = NonTerm $ \c -> x c || y c
Eps </> Eps = Eps

digitt :: Symbol
digitt = Term isDigit

idt :: Symbol
idt = Term isLetter

plust :: Symbol
plust = Term (== '+')

start :: Symbol
start = Term (== '+')

rParen :: Symbol
rParen = Term (== '(')

lParen :: Symbol
lParen = Term (== ')')

exprn :: Symbol
exprn = digitt </> (digitt <+> (Term (== '+') </> Term (== '-')) <+> digitt)

first :: [Symbol] -> [Symbol]
first [] = []
first (Eps:ss) = Eps:first ss
first ((Term t):ss) = [Term t]
first ((NonTerm t):ss) =
  let f = first [NonTerm t] in
  if Eps `elem` f then
    f ++ first ss
  else
    f

newtype Item = Item ([Symbol], [Symbol]) Symbol

newtype State = State [Item]

data Action =
    Shift State
  | Reduce [Symbol] Symbol
  | Error String
  | Accept

action :: State -> Char -> Action
action (State []) _ = Error "empty state"
action (State x:xs) c =
  let (Item (b, b') h) = x in
    case b' of
      ((Term t):tx) -> Shift $ closure 
      _ -> action (State xs) c

closure :: [Item] -> [Item]
closure [] = []
closure (x:xs) =  : closure xs




---------- Code Generator ----------

generate :: Expr -> String
generate e = generate' e ++ "\tret"

generate' :: Expr -> String
generate' (Basic (Str s)) = generateStringfn s
generate' (Basic (Number n)) = generateIntfn n
generate' (Bin (Basic (Number lhs)) (BinOp o) (Basic (Number rhs))) = generateIntfn lhs ++ generateOp o rhs
generate' (Bin e (BinOp o) (Basic (Number rhs))) = generate' e ++ generateOp o rhs
generate' e = error $ "unexpected token: " ++ show e

generateStringfn :: String -> String
generateStringfn s = foldl1 (\acc next -> acc ++ "\n" ++ next) ["\t.data", ".mydata:", "\t.string \"" ++ s ++ "\"", "\t.text", "\t.global _stringfn", "_stringfn:", "\tlea .mydata(%rip), %rax\n"]

generateIntfn :: Int -> String
generateIntfn n = foldl1 (\acc next -> acc ++ "\n" ++ next) ["\t.text", "\t.global _intfn", "_intfn:", "\tmov $" ++ show n ++ ", %rax\n"]

generateOp :: String -> Int -> String
generateOp "+" = generateOp' "add"
generateOp "-" = generateOp' "sub"

generateOp' :: String -> Int -> String
generateOp' o rhs = "\t" ++ o ++ " $" ++ show rhs ++ ", %rax\n"
