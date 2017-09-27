module Main where

import System.IO

import Compile

main :: IO ()
main = getContents >>= \x -> case compile x of
  Right asm -> putStr asm
  Left err -> hPrint stderr err
