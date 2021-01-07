{-# LANGUAGE LambdaCase #-}
module Console (mainConsole) where

import Data.List
import Data.IORef
import FileManagerEnv
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import System.IO
import Prelude hiding (break)

import Commands
import FileManager

newtype BadCommand = BadCommand String deriving Show

instance Exception BadCommand

parseCommand :: String -> (String, [String])
parseCommand command = 
  case tokens of 
    [] -> throw $ BadCommand ""
    (x:xs) -> (x, xs)
  where tokens = words command

executeCommand :: String -> [String] -> FileManager ()
executeCommand command args = void $ do
  case command of
    "cd" -> cd args
    "ls" -> do
      files <- ls args
      liftIO $ putStrLn $ intercalate "\n" (map show $ sort files)
    "mkdir"  -> mkdir  args
    "mkfile" -> mkfile args
    "rmdir"  -> rmdir  args
    "rmfile" -> rmfile args
    "info"   -> info args >>= liftIO . print 
    "read"   -> cat args >>= liftIO . putStrLn
    "write"  -> write args 
    "find"   -> find_ args >>= \case
      Just path -> liftIO $ putStrLn path
      Nothing   -> liftIO $ putStrLn "file not found"
    _ -> throw $ BadCommand command 

wrapExceptions 
  :: FileManager m 
  -> IORef Env
  -> IO (Either SomeException m)
wrapExceptions fm envRef = do 
  env <- readIORef envRef
  try $
    do
      (modifiedEnv, res) <- runFileManager fm env
      writeIORef envRef modifiedEnv
      return res

mainConsole :: String -> IO ()
mainConsole root = void $ do
  stateRef <- newIORef (Env root "")
  forever $ void $ do
    stateBefore <- readIORef stateRef
    putStr $ getRealPathToCurrentDirFromEnv stateBefore ++ ">>"
    hFlush stdout
    rawCommand <- getLine
    let (command, args) = parseCommand rawCommand
    v <- wrapExceptions (executeCommand command args) stateRef 
    case v of 
      Left e -> liftIO $ print e
      _      -> return ()
    --stateAfter <- liftIO $ readIORef stateRef
    --liftIO $ print stateAfter

