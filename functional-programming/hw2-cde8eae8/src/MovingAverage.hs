module MovingAverage (moving) where

import Control.Monad.State
import Prelude hiding (sum)

data Window = Window { sum      :: Int 
                     , beginIt  :: [Int]
                     , endIt    :: [Int]
                     }

shiftWindow :: Int -> State Window Float
shiftWindow width = do 
  modify (\win -> Window { sum = (sum win) - head (beginIt win) + head (endIt win)
                         , beginIt = tail $ beginIt win
                         , endIt = tail $ endIt win
                         })
  gets (\s -> ((fromIntegral $ sum s) :: Float) / (fromIntegral width))

moving :: Int -> [Int] -> [Float]
moving width xs = 
  evalState 
    (sequence $ map (\w -> shiftWindow w) $ [1..width] ++ replicate (length xs - width) width) 
    (Window 0 (replicate width 0 ++ xs) xs)

  
