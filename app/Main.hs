module Main where

import Control.Monad
import System.Exit
import System.IO

import Compile

main :: IO ()
main = getContents >>= either (hPutStrLn stderr >=> const exitFailure) putStr . compile
