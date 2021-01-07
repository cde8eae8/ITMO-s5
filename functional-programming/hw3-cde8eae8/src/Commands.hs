{-# LANGUAGE FlexibleContexts #-}
module Commands 
  ( DirectoryEntry(..)
  , FileManagerError(..)
  , isDirectory
  , isFile
  , entryName
  , Info(..)
  , FileInfo(..)
  , DirectoryInfo(..)
  -- commands
  , cd
  , ls
  , mkdir
  , rmdir
  , mkfile
  , rmfile
  , find_
  , cat
  , write
  , info
  ) where

import Control.Applicative
import Control.Exception 
import Control.Monad.Extra
import Data.List
import Data.Maybe
import Data.Time.Clock
import Prelude hiding (readFile, writeFile)
import System.FilePath
import System.Path.NameManip

import FS
import FileManagerEnv

data FileManagerError = PathDoesNotExists FilePath  
                      | BadArguments [String]
                      | PathOutsideOfRoot FilePath
                      deriving Show

instance Exception FileManagerError

data DirectoryEntry = Directory String
                    | File String
                    deriving (Show, Eq, Ord)

data DirectoryInfo = DirectoryInfo { dirPath :: String 
                                   , dirPermissions :: [Permission]
                                   , dirSize :: Integer
                                   , dirNFile :: Int
                                   } deriving Eq

data FileInfo = FileInfo { filePath :: String 
                         , fileType :: String
                         , filePermissions :: [Permission]
                         , fileSize :: Integer
                         , modificationTime :: UTCTime
                         } deriving Eq

data Info = F FileInfo
          | D DirectoryInfo
          deriving (Show, Eq)

convertFileSize :: Integer -> String
convertFileSize size = show (size `div` 1024) ++ " kb " 
                        ++ show (size `mod` 1024) ++ " b"

instance Show DirectoryInfo where
  show (DirectoryInfo path permissions size nFiles) = intercalate "\n"
    [ "Directory " ++ path 
    , "  Permissions: " ++ show permissions
    , "  Size: " ++ convertFileSize size
    , "  Number of files: " ++ show nFiles
    ]

instance Show FileInfo where
  show (FileInfo path fType permissions size modTime) = intercalate "\n"
    [ "File " ++ path 
    , "  Type: " ++ fType
    , "  Permissions: " ++ show permissions
    , "  Size: " ++ convertFileSize size
    , "  Last modified: " ++ show modTime
    ]

isFile :: DirectoryEntry -> Bool
isFile (File _) = True
isFile _        = False

isDirectory :: DirectoryEntry -> Bool
isDirectory (Directory _) = True
isDirectory _             = False

entryName :: DirectoryEntry -> FilePath
entryName (Directory p) = p
entryName (File p) = p

applyRelativelyWorkingPath  :: (ModifyEnv m) => (FilePath -> m a) -> FilePath -> m a
applyRelativelyWorkingPath f path = do
  throwIfOutsideRoot path
  workDir <- getRealPathToCurrentDir 
  f (workDir </> path)

throwIfOutsideRoot :: (ModifyEnv m) => String -> m ()
throwIfOutsideRoot localPath = do
  env <- modifyEnv id
  let currentDirectory = envCurrentDirectory env
      isFromRoot = head localPath == '/'
      localPathMaybe = guess_dotdot $ currentDirectory </> localPath
  when (isNothing localPathMaybe || isFromRoot) $ 
    throw $ PathOutsideOfRoot localPath

cd :: (FS m, ModifyEnv m) => [String] -> m ()
cd [path] = do
  throwIfOutsideRoot path
  env <- modifyEnv id
  let currentDirectory = envCurrentDirectory env
      rootDirectory    = envRootDirectory env
      localPathMaybe = guess_dotdot $ currentDirectory </> path
  let localPath = fromJust localPathMaybe
  exists <- existsDirectory $ rootDirectory </> localPath
  if exists
    then setCurrentDir localPath
    else throw $ PathDoesNotExists localPath
cd args = throw $ BadArguments args

ls :: (FS m, ModifyEnv m) => [String] -> m [DirectoryEntry]
ls [] = ls ["."]
ls [localPath] = do
  throwIfOutsideRoot localPath
  workingDir <- getRealPathToCurrentDir
  let path = workingDir </> localPath
  paths <- listDirectory path
  sort . catMaybes <$> mapM (getType $ workingDir </> localPath) paths
  where 
    getType :: (Monad m, FS m) => String -> String -> m (Maybe DirectoryEntry)
    getType workingPath localPath = do
      isDir <- existsDirectory path
      isFile <- existsFile path
      return $ 
        Directory localPath <$ guard isDir <|>
        File      localPath <$ guard isFile
      where path = workingPath </> localPath
ls args = throw $ BadArguments args

mkdir :: (FS m, ModifyEnv m) => [String] -> m ()
mkdir [path] = applyRelativelyWorkingPath createDirectory path
mkdir args = throw $ BadArguments args

rmdir :: (FS m, ModifyEnv m) => [String] -> m ()
rmdir [path] = applyRelativelyWorkingPath removeDirectory path
rmdir args = throw $ BadArguments args

mkfile :: (FS m, ModifyEnv m) => [String] -> m ()
mkfile [path] = applyRelativelyWorkingPath createFile path
mkfile args = throw $ BadArguments args

rmfile :: (FS m, ModifyEnv m) => [String] -> m ()
rmfile [path] = applyRelativelyWorkingPath removeFile path
rmfile args = throw $ BadArguments args

cat :: (FS m, ModifyEnv m) => [String] -> m String
cat [path] = applyRelativelyWorkingPath readFile path
cat args = throw $ BadArguments args

write :: (FS m, ModifyEnv m) => [String] -> m ()
write [path, text] = applyRelativelyWorkingPath (flip writeFile text) path
write args = throw $ BadArguments args

find_ :: (FS m, ModifyEnv m) => [String] -> m (Maybe FilePath)
find_ [name] = do
  workingDirectory <- getRealPathToCurrentDir 
  findFile name workingDirectory 
  where 
    findFile :: (Monad m, FS m) => String -> FilePath -> m (Maybe FilePath)
    findFile name path = do
      entries <- listDirectory path
      let isTargetFile = 
            \someName -> (someName == name &&) <$> existsFile (path </> someName) 
      liftM2 (<|>) (fmap (path </>) <$> findM isTargetFile entries) 
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
find_ args = throw $ BadArguments args


getDirectorySizeRecursive :: (Monad m, FS m) => FilePath -> m Integer
getDirectorySizeRecursive path = do
  entries <- map (path </>) <$> listDirectory path
  files <- filterM existsFile entries
  dirs  <- filterM existsDirectory entries
  fileSizes <- foldl' (+) 0 <$> mapM getFileSize files 
  dirSizes <- foldl' (+) 0 <$> mapM getDirectorySizeRecursive dirs
  return $ fileSizes + dirSizes

info :: (FS m, ModifyEnv m) => [String] -> m Info
info [localPath] = flip applyRelativelyWorkingPath localPath $ 
  \path -> do
    isFile <- existsFile path
    if isFile
    then do
      size <- getFileSize path
      permissions <- getPermissions path
      let extension = takeExtension path
      modificationTime <- getModificationTime path
      return $ F $ FileInfo path extension permissions size modificationTime
    else do
      size <- getDirectorySizeRecursive path
      nFiles <- length <$> 
        ((fmap (path </>) <$> listDirectory path) >>= filterM existsFile)
      permissions <- getPermissions path
      return $ D $ DirectoryInfo path permissions size nFiles
info args = throw $ BadArguments args
