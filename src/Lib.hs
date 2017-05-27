module Lib
    ( someFunc
    ) where

import Data.Char
import Control.Monad

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
  | isDigit c = compileNumber (read [c])
  | c == '"' = compileString
  | otherwise = error $ "unexpected character: '" ++ [c] ++ "' (" ++ (show $ ord c) ++ ")"

compileNumber :: Int -> IO ()
compileNumber n = do
  n' <- readNumber n
  putStrLn $ foldl1 (\acc next -> acc ++ "\n" ++ next) ["\t.text", "\t.global _intfn", "_intfn:", "\tmov $" ++ show n' ++ ", %rax", "\tret"]

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
compileString = do return ()
