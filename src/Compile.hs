module Compile where

import Safe

import qualified Data.Foldable as Foldable
import qualified Data.Map.Lazy as Map

import qualified Gen
import Parser
import Scanner
import qualified Inter

compile :: String -> Either String String
compile s = case scan s of
            Left err -> Left $ show err
            Right tokens -> let
                              quads = reverse $ parse grammar tokens
                              asm = Foldable.foldl ((++) . (++ "\n")) "" . Gen.codeToString . fst . Gen.generate $ quads
                            in
                              return . foldl1 (++) $
                              [ ".text\n"
                              , ".global _intfn\n"
                              , "_intfn:"
                              , asm
                              , "\nret\n"
                              ]

grammar :: Grammar
grammar = extend $ Grammar (Var "expr") $ Map.fromList
  [ "expr" >:> [ refer "expr", Term Add, Term Num] ||| (\xs -> (Inter.Arith Inter.Add, xs`at`0, xs`at`2))
  , "expr" >:> [ refer "expr", Term Sub, Term Num] ||| (\xs -> (Inter.Arith Inter.Sub, xs`at`0, xs`at`2))
  , "expr" >:> [ Term Num ] ||| (\xs -> (Inter.NOP, xs`at`0, Inter.Nil))
  ]
