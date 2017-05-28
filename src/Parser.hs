module Parser
    (
    ) where

---------- LR Syntax Analysis ----------

type Table = Int -> Token -> Action

data Token =
    Term String
  | NonTerm String
  | EndPoint
  deriving (Show)

data Action =
    Shift Int
  | Reduce Int Token
  | Error String
  | Accept
  | Goto Int

accept :: [Token] -> Table -> Bool
accept tok tab = analyze tab [0] $ tok ++ [EndPoint]

analyze :: Table -> [Int] -> [Token] -> Bool
analyze tab (x:xs) (s:ss) = case tab x s of
  Shift n -> analyze tab (n:x:xs) ss
  Reduce n t -> analyze tab (drop n (x:xs)) (t:s:ss)
  Error msg -> False
  Accept -> True
  Goto n -> analyze tab (n:x:xs) (s:ss)

tableExample :: Table
tableExample 0 (NonTerm "expr") = Accept
tableExample 0 (Term "+") = Shift 1
tableExample 1 (Term "1") = Shift 2
tableExample 2 (Term "2") = Shift 3
tableExample 3 EndPoint = Reduce 3 $ NonTerm "expr"
tableExample n t = error $ "state " ++ show n ++ ": unexpected " ++ show t
