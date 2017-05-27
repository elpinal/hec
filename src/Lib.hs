module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn $ foldl1 (\acc next -> acc ++ "\n" ++ next) ["\t.text", "\t.global mymain", "mymain:", "\tmov $42, %eax", "\tret"]
