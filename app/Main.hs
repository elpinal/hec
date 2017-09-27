module Main where

import System.IO

import Compile

main :: IO ()
main = getContents >>= either (hPutStr stderr) putStr . compile
