module Inter
    ( ThreeAddr
    , Op(..)
    , Operand(..)
    , Addr
    ) where

type ThreeAddr = (Result, Op, Operand, Operand)

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
