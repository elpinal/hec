module Inter
    ( Quad
    , Triple
    , Addr
    , Result(..)
    , Operator(..)
    , Op(..)
    , Operand(..)
    , toQuad
    ) where

type Quad = (Result, Op, Operand, Operand)

type Triple = (Op, Operand, Operand)

type Addr = Int

data Result = Point Addr
            | RET
            deriving (Show, Eq)

data Operator = Add
              | Sub
              | Mul
              | Quo
              deriving (Show, Eq)

data Op = Arith Operator
        | NOP
        deriving (Show, Eq)

data Operand = At Addr
             | Const Int
             | Nil
             deriving (Show, Eq)

toQuad :: Result -> Triple -> Quad
toQuad result (op, operand1, operand2) = (result, op, operand1, operand2)
