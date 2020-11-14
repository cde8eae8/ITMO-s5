module Arithmetic (Expression(..), eval, ArithmeticError(..)) where

data Expression = Number Int
                | Sum Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Pow Expression Expression deriving (Eq, Show)

data ArithmeticError = DivisionByZero | NegativePower deriving (Eq, Show)

evalExpr :: (Int -> Int -> Either ArithmeticError Int) 
         -> Expression -> Expression -> Either ArithmeticError Int
evalExpr f l r = do 
  lres <- eval l
  rres <- eval r
  f lres rres

eval :: Expression -> Either ArithmeticError Int
eval (Number x) = Right x
eval (Sum l r) = evalExpr (\a b -> Right $ a + b) l r
eval (Mul l r) = evalExpr (\a b -> Right $ a * b) l r
eval (Sub l r) = evalExpr (\a b -> Right $ a - b) l r
eval (Div l r) = evalExpr (\a b -> if b == 0 then Left DivisionByZero else Right $ a `div` b) l r
eval (Pow l r) = evalExpr (\a b -> if b < 0 then Left NegativePower else Right $ a ^ b) l r

