{-# LANGUAGE FlexibleInstances #-}
module FS (FS(..), Permission(..)) where

import Data.Time.Clock

data Permission = Read 
                | Write
                | Execute
                deriving (Eq, Show)

class FS m where
  createDirectory :: FilePath -> m ()

  createFile :: FilePath -> m ()
  
  removeDirectory :: FilePath -> m ()

  removeFile :: FilePath -> m ()

  existsFile :: FilePath -> m Bool

  existsDirectory :: String -> m Bool

  listDirectory :: FilePath -> m [FilePath]

  readFile :: FilePath -> m String

  writeFile :: FilePath -> String -> m ()

  getPermissions :: FilePath -> m [Permission]

  getModificationTime :: FilePath -> m UTCTime

  getFileSize :: FilePath -> m Integer

