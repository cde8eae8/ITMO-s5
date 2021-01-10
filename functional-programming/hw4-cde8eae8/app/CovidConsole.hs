module CovidConsole
  ( runConsole
  ) where

import Data.Maybe
import Control.Applicative
import Text.Read

import Covid

consoleUsage :: IO ()
consoleUsage = putStrLn "stack run covid width height n-iterations seed"

runConsole :: [String] -> IO ()
runConsole [wS, hS, nIterationsS, seedS] = fromJust $ (do
  w <- readMaybe wS 
  h <- readMaybe hS
  nIterations <- readMaybe nIterationsS
  seed <- readMaybe seedS
  return $ mapM_ print $ runConsoleImpl infectionInfo w h nIterations seed) 
    <|> Just consoleUsage
  where 
    infectionInfo = 
      InfectionInfo 
        { infectionP = 0.1
        , incubationPeriod = 4
        , infectiousPeriod = 5
        , immunityPeriod = 10
        }
runConsole _ = consoleUsage

runConsoleImpl :: InfectionInfo -> Int -> Int -> Int -> Int -> [CovidGrid]
runConsoleImpl infection w h nIterations seed = 
  take nIterations (iterate nextGrid (single infection w h seed))

