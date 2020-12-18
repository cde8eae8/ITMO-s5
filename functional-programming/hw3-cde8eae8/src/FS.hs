{-# LANGUAGE FlexibleInstances #-}
module FS (FS(..)) where

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

