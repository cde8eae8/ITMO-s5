{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module MockFS where 
import FS
import Debug.Trace
import FileManagerEnv
import Control.Monad.ST
import Control.Exception
import Data.STRef
import Data.Maybe
import Control.Monad.Reader
import Data.Tree
import Data.Tree.Zipper
import System.FilePath

data FileSystemEntry = Directory String 
                     | File String
                     deriving Show

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
entryName (Directory name) = name
entryName (File name) = name

data MockEnv = MockEnv { env :: Env 
                       , filesystem :: FileSystemTree
                       } deriving Show

type FSZipper a = TreePos a FileSystemEntry

--newtype FileSystemTree = FS (Tree FileSystemEntry) 
newtype FileSystemTree = FS (FSZipper Full)

instance Show FileSystemTree where
  show (FS tree) = drawTree $ fmap show (toTree tree)

type MockFS s = ReaderT (STRef s MockEnv) (ST s)

emptyFS :: FileSystemTree
emptyFS = FS $ fromTree $ (Node $ Directory "/") []

-- TODO support paths starting from '/'
-- TODO correct handling of .. and . at the begin of path
focusPath :: FSZipper Full -> FilePath -> Maybe (FSZipper Full)
focusPath startPos originalPath = 
  if head path == "/"
    then focusChild (root startPos) (tail path)
    else focusChild startPos path
  where 
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
  let focusedPath = focusPath pos path in
    fromMaybe (throw (PathDoesNotExist path)) focusedPath

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

-- TODO: prism??
mockRemoveDirectory :: FilePath -> FileSystemTree -> FileSystemTree
mockRemoveDirectory path (FS tree) = FS $
  if isDirectoryFocused focusedPath 
    then fromMaybe (root tree) (parent $ delete focusedPath)
    else throw (IsNotDirectory path)
  where
    focusedPath :: FSZipper Full
    focusedPath = throwIfPathDoesNotExistFromRoot tree path

mockRemoveEntry 
  :: (FSZipper Full -> Bool) 
  -> (FilePath -> MockFSExceptions)
  -> FilePath 
  -> FileSystemTree 
  -> FileSystemTree
mockRemoveEntry predicat exception path (FS tree) = FS $
  if predicat focusedPath 
    then fromMaybe (root tree) (parent $ delete focusedPath)
    else throw (exception path)
  where
    focusedPath :: FSZipper Full
    focusedPath = throwIfPathDoesNotExistFromRoot tree path

--findFile :: FileSystemTree -> FilePath -> File
--findFile tree path = findImpl tree (splitPath path)

--runMockFS :: ST RealWorld m -> IO m
--runMockFS st = stToIO st

runMockFS :: Env -> FileSystemTree -> MockFS RealWorld m -> IO (MockEnv, m)
runMockFS state init fs = stToIO $ do
  envRef <- newSTRef (MockEnv state init)
  v <- runReaderT fs envRef
  newState <- readSTRef envRef
  return (newState, v)

-- TODO: Lens?...
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

instance FS (MockFS s) where
  createDirectory path = do
    let (dir, name) = splitFileName $ dropTrailingPathSeparator path
    modifyFS (mockCreateEntry (Node (Directory name) []) dir name)

  createFile path = do
    let (dir, name) = splitFileName $ dropTrailingPathSeparator path
    modifyFS (mockCreateEntry (Node (File name) []) dir name)

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
      Just focus -> pure $ 
        entryName . label . fromJust <$> 
          takeWhile isJust (iterate (>>= next) (firstChild focus))

  removeFile path = do
    modifyFS $ mockRemoveEntry isFileFocused IsNotFile path 

  removeDirectory path = do
    modifyFS $ mockRemoveEntry isDirectoryFocused IsNotDirectory path 


