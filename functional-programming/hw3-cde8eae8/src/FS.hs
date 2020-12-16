{-# LANGUAGE FlexibleInstances #-}
module FS (FS(..)) where

class FS m where
  createDirectory :: FilePath -> m ()

  existsDirectory :: String -> m Bool

  existsFile :: FilePath -> m Bool

  listDirectory :: FilePath -> m [FilePath]

