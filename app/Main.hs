module Main where

import Compile

main :: IO ()
main = do
  interact $ \x -> case compile x of
             Right asm -> asm
             Left err -> show err
