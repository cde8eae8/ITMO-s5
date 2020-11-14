module NaturalNumbers ( Nat(..) 
                      , add
                      , mul
                      , sub
                      , div
                      , mod
                      , fromInt
                      , toInt
                      , isEven
                      , isOdd) where 

import Prelude hiding (div, mod)

data Nat = Z | S Nat deriving Show


instance Eq Nat where
  (==) Z (S x)     = False
  (==) (S x) Z     = False
  (==) Z Z         = True
  (==) (S x) (S y) = x == y

instance Ord Nat where
  (<=) Z _            = True
  (<=) _ Z            = False
  (<=) (S x) (S y)    = x <= y

add:: Nat -> Nat -> Nat
add Z y     = y
add (S x) y = S (add x y)

mul :: Nat -> Nat -> Nat
mul Z y = Z
mul (S x) y = add y (mul x y)

fromInt :: Int -> Nat
fromInt 0 = Z
fromInt x = S (fromInt (x - 1))

toInt :: Nat -> Int
toInt Z     = 0
toInt (S x) = 1 + (toInt x)

sub :: Nat -> Nat -> Nat
sub x Z         = x
sub Z _         = Z
sub (S x) (S y) = sub x y

isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S x)) = isEven x

isOdd :: Nat -> Bool
isOdd x = not $ isEven x

div :: Nat -> Nat -> Nat
div a Z       = error "Division by zero"
div a (S Z)   = a
div a b       
  | a == b    = S Z
  | r == Z    = Z
  | otherwise = S $ div r b
  where r = sub a b

mod :: Nat -> Nat -> Nat
mod a b = sub a $ mul (div a b) b
