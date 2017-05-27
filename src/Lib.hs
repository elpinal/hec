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




---------- Parsec -----------

data Token =
    Number Int
  | Ident String
  | Str String
  | BinOp String
  | WhiteSpace
    deriving (Show, Eq)

parseElacht :: String -> Either ParseError [Token]
parseElacht = parse lexeme "<unknown>"

lexeme :: GenParser Char st [Token]
lexeme = do
  expr <- many parseExpr
  eof
  return expr

parseExpr :: GenParser Char st Token
parseExpr =
  (many1 digit >>= return . Number . read)
  <|> (many1 (letter <|> digit) >>= return . Ident)
  <|> (parseString >>= return . Str)
  <|> (oneOf "+-" >>= return . BinOp . (:[]))
  <|> (many1 space >> return WhiteSpace)

parseString :: GenParser Char st String
parseString = do
  char '"'
  s <- many (noneOf "\"")
  char '"'
  return s
