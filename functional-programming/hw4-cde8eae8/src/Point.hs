{-# LANGUAGE BangPatterns #-}
module Point
  ( Point(..)
  , fromXY
  , plus
  , minus
  , length
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  )  where

import Prelude hiding (length)
import qualified Data.List.NonEmpty as N

data Point = Point
  { x :: !Int
  , y :: !Int
  }

instance Show Point where
  show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

fromXY :: (Int, Int) -> Point
fromXY (x, y) = Point x y

plus :: Point -> Point -> Point
plus l r = Point (x l + x r) (y l + y r)

minus         :: Point -> Point -> Point
minus l r = Point (x l - x r) (y l - y r)

scalarProduct :: Point -> Point -> Int
scalarProduct l r = (x l * x r) + (y l * y r)

crossProduct  :: Point -> Point -> Int
crossProduct l r = (x l * y r) - (y l * x r) 

length :: Point -> Point -> Double
length l r = sqrt (fromIntegral $ scalarProduct p p)
  where 
    p = minus l r

perimeter :: [Point] -> Double -- Считает периметр
perimeter points = cycleSum 0.0 length points

doubleArea :: [Point] -> Int   -- Считает удвоенную площадь
doubleArea points = cycleSum 0 crossProduct points

cycleSum :: (Num b) => b -> (a -> a -> b) -> [a] -> b
cycleSum _ f list@(x : _) = helper f list x
  where
    helper :: (Num b) => (a -> a -> b) -> [a] -> a -> b
    helper f (l : tail@(r:_)) head = 
      let !res = (+) (f l r) $! helper f tail head in res
    helper f [tail] head = 
      let !res = f head tail in res

cycleSum def _ [] = def
