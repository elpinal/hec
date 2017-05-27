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
