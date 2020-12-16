module FileManagerEnv 
  ( Env(..)
  , ModifyEnv(..)
  , setCurrentDir
  , getRealPathToCurrentDir
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
  return $ envRootDirectory env </> envCurrentDirectory env
