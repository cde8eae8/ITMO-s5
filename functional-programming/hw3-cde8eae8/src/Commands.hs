{-# LANGUAGE FlexibleContexts #-}
module Commands 
  ( FileManagerError(..)
  , cdCommand
  , lsCommand
  , mkdirCommand
  , rmdirCommand
  , mkfileCommand
  , rmfileCommand
  , findCommand
  , catCommand
  , writeCommand
  ) where

import Control.Exception 
import Control.Applicative
import Control.Monad.Extra
import Data.Maybe
import Data.List
import FS
import FileManagerEnv
import System.FilePath
import Prelude hiding (readFile, writeFile)

data FileManagerError = PathDoesNotExists FilePath
                      | BadArguments [String]
                      deriving Show

instance Exception FileManagerError

data DirectoryEntry = Directory String
                    | File String
                    deriving (Show, Eq, Ord)


applyRelativelyWorkingPath  :: (ModifyEnv m) => (FilePath -> m a) -> FilePath -> m a
applyRelativelyWorkingPath f path = do
  workDir <- getRealPathToCurrentDir 
  f (workDir </> path)

cdCommand :: (FS m, ModifyEnv m) => [String] -> m ()
cdCommand [path] = flip applyRelativelyWorkingPath path $
    \realPath -> do
      exists <- existsDirectory realPath
      if exists
        then setCurrentDir realPath
        else throw $ PathDoesNotExists realPath
cdCommand args = throw $ BadArguments args

lsCommand :: (FS m, ModifyEnv m) => [String] -> m [FilePath]
lsCommand [] = lsCommand ["."]
lsCommand [path] = 
  flip applyRelativelyWorkingPath path $ \path -> do
    paths <- listDirectory path -- paths :: [FilePath]
    catMaybes <$> mapM 
      (\path -> if existsFile path then Just path else Nothing)
      paths
    return paths
    --catMaybes <$> mapM (\path -> if existsFile path then Just path else Nothing)
    --  <$> listDirectory path
    
lsCommand args = throw $ BadArguments args

mkdirCommand :: (FS m, ModifyEnv m) => [String] -> m ()
mkdirCommand [path] = applyRelativelyWorkingPath createDirectory path
mkdirCommand args = throw $ BadArguments args

rmdirCommand :: (FS m, ModifyEnv m) => [String] -> m ()
rmdirCommand [path] = applyRelativelyWorkingPath removeDirectory path
rmdirCommand args = throw $ BadArguments args

mkfileCommand :: (FS m, ModifyEnv m) => [String] -> m ()
mkfileCommand [path] = applyRelativelyWorkingPath createFile path
mkfileCommand args = throw $ BadArguments args

rmfileCommand :: (FS m, ModifyEnv m) => [String] -> m ()
rmfileCommand [path] = applyRelativelyWorkingPath removeFile path
rmfileCommand args = throw $ BadArguments args

catCommand :: (FS m, ModifyEnv m) => [String] -> m String
catCommand [path] = applyRelativelyWorkingPath readFile path
catCommand args = throw $ BadArguments args

writeCommand :: (FS m, ModifyEnv m) => [String] -> m ()
writeCommand [path, text] = applyRelativelyWorkingPath (flip writeFile text) path
writeCommand args = throw $ BadArguments args

findCommand :: (FS m, ModifyEnv m) => [String] -> m (Maybe FilePath)
findCommand [name] = do
  workingDirectory <- getRealPathToCurrentDir 
  findFile name workingDirectory 
  where 
    findFile :: (Monad m, FS m) => String -> FilePath -> m (Maybe FilePath)
    findFile name path = do
      entries <- listDirectory path
      let isTargetFile = 
            \someName -> (someName == name &&) <$> existsFile (path </> someName) 
      liftM2 (<|>) (findM isTargetFile entries) 
                   (recursiveFoundFileCall path entries)
      where
        recursiveFoundFileCall 
          :: (Monad m, FS m) 
          => String
          -> [FilePath] 
          -> m (Maybe FilePath)
        recursiveFoundFileCall path entries = do
          dirs <- filterM existsDirectory ((path </>) <$> entries)
          join . find isJust <$> mapM (findFile name) dirs 
findCommand args = throw $ BadArguments args
