module MovingAverageTest (tests) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Text.Printf

import MovingAverage

test :: Int -> [Int] -> [Float] -> Spec
test w testSet expected = it ("moving " ++ (show w) ++ " " ++ (show testSet) ++ " -> " ++ (show expected)) $
  moving w testSet `shouldBe` expected

tests :: IO TestTree
tests = testSpec "" $ do
  describe "MovingAverage" $ do
      test 1 [1, 2, 3] [1.0, 2.0, 3.0]
      test 2 [1, 2, 3] [1.0, 1.5, 2.5]
      test 4 [1, 5, 3, 8, 7, 9, 6] [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
      test 2 [1, 5, 3, 8, 7, 9, 6] [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
      test 1 [1] [1]
      test 2 [1, 2] [1, 1.5]
      test 2 [] []
      test 2 [1] [1]
      test 1 [1] [1]
      test 1 [] []
      test 100000 [] []

