module ArithmeticTests (tests) where

import Test.Hspec
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import Arithmetic
import Control.Monad
import Text.Printf

import Test.Tasty
import Test.Tasty.Hspec
import Test.HUnit (assertEqual)

test :: Either ArithmeticError Int -> Expression -> Spec
test expected testSet = it (show testSet ++ " -> " ++ show expected) $ assertEqual "" expected (eval testSet)

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
        
