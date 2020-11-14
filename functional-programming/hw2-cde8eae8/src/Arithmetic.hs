module Arithmetic 
  ( Expression(..)
  , ArithmeticError(..)
  , eval
  ) where

data Expression = Number Int
                | Sum Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Pow Expression Expression deriving (Eq, Show)

data ArithmeticError = DivisionByZero | NegativePower deriving (Eq, Show)

evalExpr 
  :: Expression 
  -> Expression 
  -> (Int -> Int -> Either ArithmeticError Int) 
  -> Either ArithmeticError Int
evalExpr l r f = do 
  lres <- eval l
  rres <- eval r
  f lres rres

eval :: Expression -> Either ArithmeticError Int
eval (Number x) = Right x
eval (Sum l r) = evalExpr l r (\a b -> Right $ a + b)
eval (Mul l r) = evalExpr l r (\a b -> Right $ a * b) 
eval (Sub l r) = evalExpr l r (\a b -> Right $ a - b) 
eval (Div l r) = evalExpr l r (\a b -> if b == 0 
                                   then Left DivisionByZero 
                                   else Right $ a `div` b) 
eval (Pow l r) = evalExpr l r (\a b -> if b < 0 
                                   then Left NegativePower 
                                   else Right $ a ^ b)

