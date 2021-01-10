module Main where

import Data.Maybe
import Data.List
import System.Environment

import qualified CovidConsole

import Halyava 
import HalyavaShow

usage :: String
usage = "stack run covid|halyava"

runHalyava :: IO ()
runHalyava = do
  runInterpret (fibNumbers 0) 
  runInterpret (fibNumbers 1)
  runInterpret (fibNumbers 2)
  runInterpret (fibNumbers 10)
  putStrLn ""
  putStrLn "Code for n = 10:"
  putStrLn $ runShow (fibNumbers 10)

main :: IO ()
main = do
  args <- uncons <$> getArgs
  if isNothing args
  then 
    putStrLn usage
  else do
    let (command, commandArgs) = fromJust args
    case command of 
      "covid"   -> CovidConsole.runConsole commandArgs
      "halyava" -> runHalyava
      _       -> putStrLn usage

