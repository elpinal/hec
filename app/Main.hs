module Main where

import Control.Monad
import System.Exit
import System.IO

import Compile

main :: IO ()
main = getContents >>= either (hPutStr stderr >=> const exitFailure) putStr . compile
