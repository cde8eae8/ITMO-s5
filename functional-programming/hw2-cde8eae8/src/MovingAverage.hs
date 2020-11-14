module MovingAverage 
  ( moving
  ) where

import Prelude hiding (sum)
import Control.Monad.State

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
  v <- gets (fromIntegral . sum)
  return $ v / (fromIntegral width)

moving :: Int -> [Int] -> [Float]
moving width xs =
  evalState 
    (mapM (\w -> shiftWindow w) $ 
      [1..min width len] ++ replicate (len - width) width) 
    (Window 0 (replicate width 0 ++ xs) xs)
  where len = length xs

