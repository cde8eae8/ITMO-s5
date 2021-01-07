{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment

import Console (mainConsole)
import GUI (mainGUI)

usage :: IO ()
usage = putStrLn "Usage: stack run gui|console [root]"

start :: String -> String -> IO ()
start "gui" path = mainGUI path
start "console" path = mainConsole path
start _ _ = usage


main :: IO ()
main = do
  getArgs >>= 
    \case 
      [interface] -> start interface "."
      [interface, root] -> start interface root
      _ -> usage

