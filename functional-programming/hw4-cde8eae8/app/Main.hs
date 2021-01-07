module Main where

import Data.Maybe
import Data.List
import Control.Monad.State
import System.Environment

import qualified CovidConsole

import Halyava 
import HalyavaShow


main :: IO ()
main = do
  args <- uncons <$> getArgs
  if (isNothing args) 
  then 
    (putStrLn "expected argument")
  else do
    let (command, commandArgs) = fromJust args
    case command of 
      "covid" -> CovidConsole.runConsole commandArgs
    --let v = runInterpret example2
    runInterpret (fibNumbers 0) 
    runInterpret (fibNumbers 1)
    runInterpret (fibNumbers 2)
    runInterpret (fibNumbers 10)

  putStrLn $ runShow (fibNumbers 10)
