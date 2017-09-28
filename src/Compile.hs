module Compile where

import Safe

import qualified Data.Map.Lazy as Map

import qualified Gen
import Parser
import Scanner
import qualified Inter

compile :: String -> Either String String
compile = either (Left . show) f . scan
  where
    f :: [Token] -> Either String String
    f tokens = return $ unlines
                 [ ".text"
                 , ".global _intfn"
                 , "_intfn:" ++ asm tokens
                 , "ret"
                 ]

    quads :: [Token] -> [Inter.Quad]
    quads = reverse . parse grammar

    asm :: [Token] -> String
    asm = foldl joinWithNL "" . Gen.codeToString . fst . Gen.generate . quads

    joinWithNL :: String -> String -> String
    joinWithNL s t = s ++ "\n" ++ t

grammar :: Grammar
grammar = extend (Var "expr") $ Map.fromList
  [ "expr" >:> [ refer "expr", Term Add, refer "term"] ||| (\xs -> (Inter.Arith Inter.Add, xs`at`0, xs`at`2))
  , "expr" >:> [ refer "expr", Term Sub, refer "term"] ||| (\xs -> (Inter.Arith Inter.Sub, xs`at`0, xs`at`2))
  , "expr" >:> [ refer "term" ] ||| (\xs -> (Inter.NOP, xs`at`0, Inter.Nil))
  , "term" >:> [ refer "term", Term Mul, Term Num] ||| (\xs -> (Inter.Arith Inter.Mul, xs`at`0, xs`at`2))
  , "term" >:> [ Term Num ] ||| (\xs -> (Inter.NOP, xs`at`0, Inter.Nil))
  ]
