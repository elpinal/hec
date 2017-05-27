module Lib
    ( compile
    ) where

import Data.Char
import Control.Monad

import Text.ParserCombinators.Parsec

someFunc :: IO ()
someFunc = do
  val <- getLine
  when ((not $ null val) && (all isDigit val))
    $ putStrLn $ foldl1 (\acc next -> acc ++ "\n" ++ next) ["\t.text", "\t.global _mymain", "_mymain:", "\tmov $" ++ val ++ ", %eax", "\tret"]

compile :: IO ()
compile = do
  c <- getChar
  compile' c

compile' :: Char -> IO ()
compile' c
  | isDigit c = compileExpr (read [c])
  | c == '"' = compileString
  | otherwise = error $ "unexpected character: '" ++ [c] ++ "' (" ++ (show $ ord c) ++ ")"

compileExpr :: Int -> IO ()
compileExpr n = do
  n' <- readNumber n
  putStrLn $ foldl1 (\acc next -> acc ++ "\n" ++ next) ["\t.text", "\t.global _intfn", "_intfn:", "\tmov $" ++ show n' ++ ", %rax"]
  compileExpr'

compileExpr' :: IO ()
compileExpr' = do
  c <- skipSpace
  if isControl c then
    putStrLn "\tret"
  else do
    compileOp c

compileOp :: Char -> IO ()
compileOp '+' = op "add"
compileOp '-' = op "sub"
compileOp c = error $ "expected operator, but got '" ++ [c] ++ "'"

op :: String -> IO ()
op s = do
  c <- skipSpace
  guard . isDigit $ c
  n <- readNumber $ read [c]
  putStrLn $ '\t' : s ++ " $" ++ show n ++ ", %rax"


skipSpace :: IO Char
skipSpace = do
  c <- getChar
  if isSpace c then
    skipSpace
  else
    return c

readNumber :: Int -> IO Int
readNumber initial = do
  x <- getChar
  readNumber' initial x
    where
      readNumber' :: Int -> Char -> IO Int
      readNumber' initial x
        | isDigit x = readNumber (initial*10 + read [x])
        | isSpace x = return initial
        | isControl x = return initial
        | otherwise = error $ "unexpected character in number: '" ++ [x] ++ "' (" ++ (show $ ord x) ++ ")"

compileString :: IO ()
compileString = do
  s <- readString ""
  putStrLn $ foldl1 (\acc next -> acc ++ "\n" ++ next) ["\t.data", ".mydata:", "\t.string \"" ++ s ++ "\"", "\t.text", "\t.global _stringfn", "_stringfn:", "\tlea .mydata(%rip), %rax", "\tret"]

readString :: String -> IO String
readString acc = do
  c <- getChar
  readString' acc c
    where
      readString' :: String -> Char -> IO String
      readString' acc c
        | c == '"' = return acc
        | otherwise = readString $ acc ++ [c]




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




---------- Code Generator ----------

generate :: Expr -> String
generate (Basic (Str s)) = generateStringfn s
generate (Basic (Number n)) = generateIntfn n
generate (Bin (Basic (Number lhs)) (BinOp o) (Basic (Number rhs))) = generateIntfn' lhs ++ generateOp o rhs

generateStringfn :: String -> String
generateStringfn s = foldl1 (\acc next -> acc ++ "\n" ++ next) ["\t.data", ".mydata:", "\t.string \"" ++ s ++ "\"", "\t.text", "\t.global _stringfn", "_stringfn:", "\tlea .mydata(%rip), %rax", "\tret"]

generateIntfn :: Int -> String
generateIntfn n = generateIntfn' n ++ "\tret"

generateIntfn' :: Int -> String
generateIntfn' n = foldl1 (\acc next -> acc ++ "\n" ++ next) ["\t.text", "\t.global _intfn", "_intfn:", "\tmov $" ++ show n ++ ", %rax\n"]

generateOp :: String -> Int -> String
generateOp "+" = generateOp' "add"
generateOp "-" = generateOp' "sub"

generateOp' :: String -> Int -> String
generateOp' o rhs = o ++ "$" ++ show rhs ++ "%rax\n"
