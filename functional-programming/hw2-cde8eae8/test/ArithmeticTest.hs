module ArithmeticTest (tests) where

import Control.Monad
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Text.Printf

import Arithmetic

test :: Either ArithmeticError Int -> Expression -> Spec
test expected testSet = it (show testSet ++ " -> " ++ show expected) $ 
  expected `shouldBe` (eval testSet)

tests :: IO TestTree
tests = testSpec "" $ do
  describe "Arithmetic" $ do
          test (Right 4)     (Sum (Number 1) (Number 3))
          test (Right 6)     (Mul (Number 2) (Number 3))
          test (Right (-1))  (Sub (Number 2) (Number 3))
          test (Right 2)     (Div (Number 8) (Number 3))
          test (Right 4)     (Div (Number 8) (Number 2))
          test (Left DivisionByZero) (Div (Number 8) (Number 0))
          test (Right 512)   (Pow (Number 8) (Number 3))
          test (Right 1)     (Pow (Number 8) (Number 0))
          test (Left NegativePower) (Pow (Number 8) (Number (-1)))
          test (Right 121) 
             (Pow (Sum (Mul (Number 3) (Number 4)) (Sub (Number 5) (Number 6))) 
                  (Div (Number (-2)) (Number (-1))))
        
