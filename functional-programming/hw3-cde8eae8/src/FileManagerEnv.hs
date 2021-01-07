module FileManagerEnv 
  ( Env(..)
  , ModifyEnv(..)
  , setCurrentDir
  , getRealPathToCurrentDir
  , getRealPathToCurrentDirFromEnv 
  ) where

import System.FilePath

data Env = Env { envRootDirectory :: String 
               , envCurrentDirectory :: String
               } deriving Show

class (Monad m) => ModifyEnv m where
  modifyEnv :: (Env -> Env) -> m Env

setCurrentDir :: (ModifyEnv m) => String -> m () 
setCurrentDir path = do
  modifyEnv (\v -> v { envCurrentDirectory = path })
  return ()

getRealPathToCurrentDir :: (ModifyEnv m) => m String
getRealPathToCurrentDir = do
  env <- modifyEnv id 
  return $ getRealPathToCurrentDirFromEnv env

getRealPathToCurrentDirFromEnv :: Env -> String
getRealPathToCurrentDirFromEnv env = 
  let result = envRootDirectory env </> envCurrentDirectory env in
    if result == "" then "." else result

