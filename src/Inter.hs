module Inter
    ( Quad
    , Triple
    , Addr
    , Result(..)
    , Operator(..)
    , Op(..)
    , Operand(..)
    ) where

type Quad = (Result, Op, Operand, Operand)

type Triple = (Op, Operand, Operand)

type Addr = Int

data Result = Point Addr
            | RET

data Operator = Add
              | Sub
              | Mul
              | Quo

data Op = Arith Operator
        | NOP

data Operand = At Addr
             | Const Int
             | Nil
