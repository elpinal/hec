module Parser
    (
    ) where

---------- LR Syntax Analysis ----------

type Table = Int -> Symbol -> Action

data Symbol =
    Term Token
  | NonTerm String
  | EndPoint
  deriving (Show)

data Token =
    Num Int
  | Str String
  | Ident String
  | Add
  | Sub
  | Mul
  | Quo
  | LParen
  | RParen
  deriving (Show, Eq)

data Action =
    Shift Int
  | Reduce Int Symbol
  | Error String
  | Accept
  | Goto Int
  deriving (Show)

accept :: [Symbol] -> Table -> Bool
accept tok tab = analyze tab [0] $ tok ++ [EndPoint]

analyze :: Table -> [Int] -> [Symbol] -> Bool
analyze tab (x:xs) (s:ss) = case tab x s of
  Shift n -> analyze tab (n:x:xs) ss
  Reduce n t -> analyze tab (drop n (x:xs)) (t:s:ss)
  Error msg -> False
  Accept -> True
  Goto n -> analyze tab (n:x:xs) ss
analyze t xs ss = error $ "unexpected error: " ++ show xs ++ ", " ++ show ss

tableExample :: Table
tableExample 0 (Term (Num _)) = Shift 1
tableExample 0 (NonTerm "expr") = Shift 2
tableExample 0 (Term LParen) = Shift 5
tableExample 1 EndPoint = Reduce 1 $ NonTerm "expr"
tableExample 1 (Term RParen) = Reduce 1 $ NonTerm "expr"
tableExample 1 (Term t)
  | t `elem` [Add, Sub, Mul, Quo] = Reduce 1 $ NonTerm "expr"
tableExample 2 EndPoint = Accept
tableExample 2 (Term t)
  | t `elem` [Add, Sub, Mul, Quo] = Shift 3
tableExample 3 (Term (Num _)) = Shift 4
tableExample 4 EndPoint = Reduce 3 $ NonTerm "expr"
tableExample 4 (Term RParen) = Reduce 3 $ NonTerm "expr"
tableExample 4 (Term t)
  | t `elem` [Add, Sub, Mul, Quo] = Reduce 3 $ NonTerm "expr"
tableExample 5 (NonTerm "expr") = Shift 6
tableExample 5 (Term (Num _)) = Shift 1
tableExample 6 (Term t)
  | t `elem` [Add, Sub, Mul, Quo] = Shift 3
tableExample 6 (Term RParen) = Shift 7
tableExample 7 EndPoint = Reduce 3 $ NonTerm "expr"
tableExample 7 (Term RParen) = Reduce 3 $ NonTerm "expr"
tableExample 7 (Term t)
  | t `elem` [Add, Sub, Mul, Quo] = Reduce 3 $ NonTerm "expr"
tableExample n t = error $ "state " ++ show n ++ ": unexpected " ++ show t
