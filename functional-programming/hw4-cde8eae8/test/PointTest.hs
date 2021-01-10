module PointTest 
  ( tests
  , benchmarks
  ) where

import Criterion.Main
import qualified Data.List.NonEmpty as N
import Prelude hiding (length)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Point

lazyCycleSum :: (Num b) => (a -> a -> b) -> N.NonEmpty a -> b
lazyCycleSum f list@(x N.:| _) = helper f list x
  where
    helper :: (Num b) => (a -> a -> b) -> N.NonEmpty a -> a -> b
    helper f (l N.:| (r:xs)) head = f l r + helper f (r N.:| xs) head
    helper f (tail N.:| []) head = f head tail

lazyPerimeter :: [Point] -> Double -- Считает периметр
lazyPerimeter (x:xs) = lazyCycleSum length (x N.:| xs)
lazyPerimeter [] = 0.0

lazyDoubleArea :: [Point] -> Int   -- Считает удвоенную площадь
lazyDoubleArea (x:xs) = lazyCycleSum crossProduct (x N.:| xs)
lazyDoubleArea [] = 0

tests :: IO TestTree
tests = testSpec "" $ do
  describe "Base operations" $ do
    mapM_ (\(points, per, area) -> 
              it (show points) $ do
                perimeter  points `shouldBe` per 
                doubleArea points `shouldBe` area 
                lazyPerimeter  points `shouldBe` per 
                lazyDoubleArea points `shouldBe` area) 
      [ ([], 0.0, 0)
      , (fromXY <$> [(0, 0), (3, 0), (0, 4)], 12.0, 12)
      , (fromXY <$> [(0, 0), (1, 0), (1, 1), (0, 1)], 4.0, 2)
      , (fromXY <$> [(0, 0), (5, 0), (5, 5), (0, 5)], 20.0, 50)
      ]

benchmarks :: [Benchmark]
benchmarks = 
  [ bench "strict perimeter" $ nf perimeter bigPolygon 
  , bench "strict area"      $ nf doubleArea bigPolygon 
  , bench "lazy perimeter"   $ nf lazyPerimeter bigPolygon 
  , bench "lazy area"        $ nf lazyDoubleArea bigPolygon 
  ]
  where bigPolygon =  (\x -> fromXY (x, x*x)) <$> [1..10^7] 

