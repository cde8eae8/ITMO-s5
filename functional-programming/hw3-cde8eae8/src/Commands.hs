{-# LANGUAGE FlexibleContexts #-}
module Commands (FileManagerError(..), cd, ls, mkdir) where

import Control.Exception 
import Control.Monad.Except
import FS
import FileManagerEnv
import System.FilePath

data FileManagerError = PathDoesNotExists FilePath
                      | BadArguments [String]
                      deriving Show

instance Exception FileManagerError

cd 
  :: ( FS m
     , ModifyEnv m
     ) 
  => [String] 
  -> m ()
cd [path] = do
  workDir <- getRealPathToCurrentDir 
  let newPath = workDir </> path
  exists <- existsDirectory newPath
  if exists
    then setCurrentDir newPath
    else throw $ PathDoesNotExists path
cd args = throw $ BadArguments args

ls 
  :: ( FS m
     , ModifyEnv m
     ) 
  => [String] 
  -> m [FilePath]
ls [] = ls ["."]
ls [path] = do
  workDir <- getRealPathToCurrentDir 
  listDirectory (workDir </> path)
ls args = throw $ BadArguments args

mkdir
  :: ( FS m
     , ModifyEnv m
     ) 
  => [String] 
  -> m ()
mkdir [path] = do
  workDir <- getRealPathToCurrentDir 
  createDirectory (workDir </> path)
mkdir args = throw $ BadArguments args
