module Compile where

import Safe

import qualified Data.Foldable as Foldable
import qualified Data.Map.Lazy as Map

import qualified Gen
import Parser
import Scanner
import qualified Inter

compile :: String -> Either String String
compile s = either (Left . show) f $ scan s
  where
    f :: [Token] -> Either String String
    f tokens = return . foldl1 (++) $
                 [ ".text\n"
                 , ".global _intfn\n"
                 , "_intfn:"
                 , asm tokens
                 , "\nret\n"
                 ]

    quads :: [Token] -> [Inter.Quad]
    quads = reverse . parse grammar

    asm :: [Token] -> String
    asm = Foldable.foldl ((++) . (++ "\n")) "" . Gen.codeToString . fst . Gen.generate . quads

grammar :: Grammar
grammar = extend $ Grammar (Var "expr") $ Map.fromList
  [ "expr" >:> [ refer "expr", Term Add, refer "term"] ||| (\xs -> (Inter.Arith Inter.Add, xs`at`0, xs`at`2))
  , "expr" >:> [ refer "expr", Term Sub, refer "term"] ||| (\xs -> (Inter.Arith Inter.Sub, xs`at`0, xs`at`2))
  , "expr" >:> [ refer "term" ] ||| (\xs -> (Inter.NOP, xs`at`0, Inter.Nil))
  , "term" >:> [ refer "term", Term Mul, Term Num] ||| (\xs -> (Inter.Arith Inter.Mul, xs`at`0, xs`at`2))
  , "term" >:> [ Term Num ] ||| (\xs -> (Inter.NOP, xs`at`0, Inter.Nil))
  ]
