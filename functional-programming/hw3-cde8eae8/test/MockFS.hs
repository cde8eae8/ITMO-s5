{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module MockFS 
  ( MockFS(..)
  , runMockFS
  , FileInfo(..)
  , DirectoryInfo(..)
  , FileSystemEntry(..)
  , emptyFS
  , FileSystemTree(..)
  , MockEnv(..)
  , dirInfo
  , dirInfoE
  , entryName
  , fileInfo
  , fileInfoE
  , focusSubTreeFS
  , focusPathFS
  , isFile
  , isDirectory
  ) where 

import Data.Maybe
import Data.STRef
import Data.Time.Clock
import Data.Tree
import Data.Tree.Zipper
import Control.Exception
import Control.Monad.Reader
import Control.Monad.ST
import System.FilePath

import FileManagerEnv
import FS

data DirectoryInfo = DirectoryInfo { dirName :: String 
                                   , dirPermissions :: [Permission]
                                   } deriving (Show, Eq)

data FileInfo = FileInfo { fileName :: String 
                         , filePermissions :: [Permission]
                         , fileSize :: Integer
                         , modificationTime :: UTCTime
                         , fileContent :: String
                         } deriving (Show, Eq)

data FileSystemEntry = Directory DirectoryInfo
                     | File FileInfo
                     deriving (Show, Eq)


directory :: String -> FileSystemEntry
directory name = Directory $ DirectoryInfo name [Read, Write]

file :: String -> FileSystemEntry
file name = 
  File $ FileInfo 
    { fileName = name
    , filePermissions = [Read, Write]
    , fileSize = 5 
    , modificationTime = read "2020-12-08 20:10:24.069346018 UTC"
    , fileContent = "content of " ++ name
    }

data MockFSExceptions = PathDoesNotExist FilePath
                      | PathAlreadyExists FilePath
                      | IsNotDirectory FilePath
                      | IsNotFile FilePath
                      deriving Show

instance Exception MockFSExceptions

instance ModifyEnv (MockFS s) where
  modifyEnv f = do
    ref <- ask
    lift $ modifySTRef ref (\st -> st { env = f (env st) })
    newRef <- ask
    v <- lift $ readSTRef newRef
    return $ env v 

entryName :: FileSystemEntry -> String
entryName (Directory info) = dirName info
entryName (File info) = fileName info 

fileInfoE :: FileSystemEntry -> FileInfo
fileInfoE (File info) = info 
fileInfoE (Directory _) = error "works only for files"

dirInfoE :: FileSystemEntry -> DirectoryInfo
dirInfoE (File _) = error "works only for files"
dirInfoE (Directory info) = info

fileInfo :: FSZipper Full -> FileInfo
fileInfo zipper = 
  case label zipper of
    File info -> info 
    Directory _ -> error "works only for files"

dirInfo :: FSZipper Full -> DirectoryInfo
dirInfo zipper = dirInfoE (label zipper)

entryPermissions :: FSZipper Full -> [Permission]
entryPermissions zipper = 
  case label zipper of
    File info -> filePermissions info 
    Directory info -> dirPermissions info

data MockEnv = MockEnv { env :: Env 
                       , filesystem :: FileSystemTree
                       } deriving Show

type FSZipper a = TreePos a FileSystemEntry

newtype FileSystemTree = FS (FSZipper Full) deriving Eq

instance Show FileSystemTree where
  show (FS tree) = drawTree $ fmap show (toTree tree)

type MockFS s = ReaderT (STRef s MockEnv) (ST s)

emptyFS :: FileSystemTree
emptyFS = FS $ fromTree $ (Node $ directory "_rootEntry") []

focusPathFS :: FileSystemTree -> FilePath -> Maybe (FSZipper Full)
focusPathFS (FS fs) = focusPathFromRoot fs 

focusSubTreeFS :: Int -> FileSystemTree -> FilePath -> [(FilePath, FileSystemEntry)]
focusSubTreeFS maxDepth (FS fs) path = 
  focusSubTree maxDepth path (fromJust $ focusPathFromRoot fs path) 

focusSubTree :: Int -> FilePath -> FSZipper Full -> [(FilePath, FileSystemEntry)]
focusSubTree depth path pos 
  | depth == 0        = []
  | isFileFocused pos = []
  | otherwise =
      let children = takeWhile isJust (iterate (>>= next) (firstChild pos)) 
          dirs = map fromJust children in
        concat ((\focus -> 
          (path </> entryName (label focus), label focus) 
          : focusSubTree (depth - 1) (path </> entryName (label focus)) focus) <$> dirs)


focusPath :: FSZipper Full -> FilePath -> Maybe (FSZipper Full)
focusPath startPos originalPath = focusChild startPos path
  where 
    path :: [String]
    path = splitDirectories originalPath

    focusChild :: FSZipper Full -> [FilePath] -> Maybe (FSZipper Full)
    focusChild treePos (dir:dirs) = do
      nextDir <- focusNextDir treePos dir
      focusChild nextDir dirs
    focusChild treePos [] = Just treePos

    focusNextDir :: FSZipper Full -> String -> Maybe (FSZipper Full)
    focusNextDir treePos "."  = Just treePos
    focusNextDir treePos ".." = Just $ fromMaybe (root treePos) (parent treePos)
    focusNextDir treePos dir  = findInRow (firstChild treePos) dir 

    findInRow :: Maybe (FSZipper Full) -> String -> Maybe (FSZipper Full)
    findInRow posInRow name = do
      entry <- posInRow
      if entryName (label entry) == name 
        then Just entry
        else findInRow (next entry) name

focusPathFromRoot :: FSZipper Full -> FilePath -> Maybe (FSZipper Full)
focusPathFromRoot tree = focusPath (root tree) 

throwIfPathDoesNotExist :: FSZipper Full -> FilePath -> FSZipper Full
throwIfPathDoesNotExist pos path = 
    if isNothing focusedPath 
    then throw (PathDoesNotExist path)
    else fromJust focusedPath
  where focusedPath = focusPath pos path 

throwIfPathDoesNotExistFromRoot :: FSZipper Full -> FilePath -> FSZipper Full
throwIfPathDoesNotExistFromRoot tree = throwIfPathDoesNotExist (root tree) 

mockCreateEntry 
  :: Tree FileSystemEntry 
  -> FilePath 
  -> String 
  -> FileSystemTree 
  -> FileSystemTree
mockCreateEntry newEntry path name (FS tree) = 
  if isNothing focusedNewEntry
    then FS $ insert newEntry (children focusedPath)
    else throw $ PathAlreadyExists $ path </> name
  where
    focusedPath :: FSZipper Full
    focusedPath = throwIfPathDoesNotExistFromRoot tree path
    focusedNewEntry :: Maybe (FSZipper Full)
    focusedNewEntry = focusPath focusedPath name 

mockRemoveEntry 
  :: (FSZipper Full -> Bool) 
  -> (FilePath -> MockFSExceptions)
  -> FilePath 
  -> FileSystemTree 
  -> FileSystemTree
mockRemoveEntry predicat exception path (FS tree) = FS $
  if predicat (focusedPath tree path)
    then fromMaybe (root tree) (parent $ delete (focusedPath tree path))
    else throw (exception path)
  where
    focusedPath :: FSZipper Full -> String -> FSZipper Full
    focusedPath tree path = throwIfPathDoesNotExistFromRoot tree path

runMockFS :: Env -> FileSystemTree -> MockFS RealWorld m -> IO (MockEnv, m)
runMockFS state init fs = stToIO $ do
  envRef <- newSTRef (MockEnv state init)
  v <- runReaderT fs envRef
  newState <- readSTRef envRef
  return (newState, v)

modifyFS :: (FileSystemTree -> FileSystemTree) -> MockFS s ()
modifyFS f = do
    envRef <- ask
    lift $ modifySTRef envRef (\st -> st { filesystem = f $ filesystem st })

getFS :: MockFS s FileSystemTree
getFS = do
    envRef <- ask
    lift $ filesystem <$> readSTRef envRef


isDirectoryFocused :: FSZipper Full -> Bool
isDirectoryFocused z = case label z of 
                         Directory{} -> True
                         File{}      -> False

isFileFocused :: FSZipper Full -> Bool
isFileFocused z = case label z of 
                         Directory{} -> False
                         File{}      -> True

isFile :: FileSystemEntry -> Bool
isFile z = case z of 
            Directory{} -> False
            File{}      -> True

isDirectory :: FileSystemEntry -> Bool
isDirectory e = not $ isFile e

instance FS (MockFS s) where
  createDirectory path = do
    let (dir, name) = splitFileName $ dropTrailingPathSeparator path
    modifyFS (mockCreateEntry (Node (directory name) []) dir name)

  createFile path = do
    let (dir, name) = splitFileName $ dropTrailingPathSeparator path
    modifyFS (mockCreateEntry (Node (file name) []) dir name)

  existsDirectory path = do
    FS fs <- getFS
    let focus = focusPathFromRoot fs path
    return $ isJust focus && isDirectoryFocused (fromJust focus)

  existsFile path = do
    FS fs <- getFS
    let focus = focusPathFromRoot fs path
    return $ isJust focus && isFileFocused (fromJust focus)

  listDirectory path = do
    FS fs <- getFS
    let focusMaybe = focusPathFromRoot fs path
    case focusMaybe of 
      Nothing -> throw $ PathDoesNotExist path 
      Just focus -> 
        if isDirectoryFocused focus
        then
          pure $ entryName . label . fromJust <$> 
              takeWhile isJust (iterate (>>= next) (firstChild focus))
        else
          throw $ IsNotDirectory path

  removeFile path = do
    modifyFS $ mockRemoveEntry isFileFocused IsNotFile path 

  removeDirectory path = do
    modifyFS $ mockRemoveEntry isDirectoryFocused IsNotDirectory path 

  readFile path = do
    FS fs <- getFS
    exists <- existsFile path
    unless exists $ throw $ IsNotFile path
    let focus = fromJust $ focusPathFromRoot fs path
    return $ (fileContent . fileInfo) focus

  writeFile path content = do
    exists <- existsFile path
    unless exists $ createFile path
    modifyFS (\(FS fs) -> FS $
      let focus = fromJust $ focusPathFromRoot fs path in
        modifyLabel (\(File info) -> File $ info { fileContent = content }) focus)

  getPermissions path = do
    FS fs <- getFS
    let focus = focusPathFromRoot fs path
    when (isNothing focus) $ throw $ PathDoesNotExist path
    return $ entryPermissions $ fromJust focus

  getFileSize path = do
    FS fs <- getFS
    exists <- existsFile path
    unless exists $ throw $ IsNotFile path
    let focus = fromJust $ focusPathFromRoot fs path
    return $ (fileSize . fileInfo) focus

  getModificationTime path = do
    FS fs <- getFS
    exists <- existsFile path
    unless exists $ throw $ IsNotFile path
    let focus = fromJust $ focusPathFromRoot fs path
    return $ (modificationTime . fileInfo) focus

             

