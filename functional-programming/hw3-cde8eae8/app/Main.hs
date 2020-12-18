module Main where

import Commands
import FileManager
import FileManagerEnv
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.IORef
import Data.Tuple.Extra
import System.IO
import Prelude hiding (break)

parseCommand :: String -> (String, [String])
parseCommand command = 
  case x of 
    [] -> error "12312" -- TODO
    (x:xs) -> (x, xs)
  where x = words command

executeCommand :: String -> [String] -> FileManager ()
executeCommand command args = void $ do
  case command of
    "cd" -> cdCommand args
    "ls" -> do
      files <- lsCommand args
      liftIO $ putStrLn $ intercalate "\n" files
    --"mkdir" -> mkdir args
    _ -> error "bad command" -- TODO

runFS :: FileManager m -> Env -> IO (Env, m)
runFS fm env = do
  envRef <- newIORef env
  res <- runReaderT fm envRef
  env <- readIORef envRef
  return (env, res)

wrapExceptions 
  :: FileManager m 
  -> FileManager (Either SomeException m)
wrapExceptions fm = do 
  envRef <- ask
  env <- liftIO $ readIORef envRef
  liftIO $ try $
    do
      (modifiedEnv, res) <- runFS fm env
      writeIORef envRef modifiedEnv
      return res

-- А что если 
-- cd 
--   :: ( FS m                          -- file system
--      , MonadReader env m             -- has environment
--      , HasCurrentWorkingDir m String -- environment has lens for curWD
--      , HasRootWorkingDir    m String -- environment has lens for root
--      , ExceptT m
--      )
--  + прочитай статью https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/,
--  там было про линзы
-- по идее тогда все pure и мок система похранит себя в SM
-- Где ловить IO исключения?... - похоже все-таки развернуть все и свернуть
-- в момент вызова execute
--
-- Env хранит rootPath + currentWorkingDirectory
-- + у нас есть тайпклассы на них
mainLoop :: FileManager ()
mainLoop = do
  forever $ void $ do
    liftIO $ putStr ">>"
    liftIO $ hFlush stdout
    rawCommand <- liftIO getLine
    let (command, args) = parseCommand rawCommand
    v <- wrapExceptions (executeCommand command args) 
    case v of 
      Left e -> liftIO $ print e
      _      -> return ()
    stateRef <- ask
    state <- liftIO $ readIORef stateRef
    liftIO $ print state

main :: IO ()
main = void $ do
  runFS mainLoop (Env "" "")

