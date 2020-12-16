{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module MockFS where 
import FS
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

newtype FileSystemTree = FS (Tree FileSystemEntry) 

type FSZipper a = TreePos a FileSystemEntry

instance Show FileSystemTree where
  show (FS tree) = drawTree $ fmap show tree

type MockFS s = ReaderT (STRef s MockEnv) (ST s)

emptyFS :: FileSystemTree
emptyFS = FS $ (Node $ Directory "/") []

-- TODO support paths starting from '/'
focusPath :: FSZipper Full -> [FilePath] -> Maybe (FSZipper Full)
focusPath rootPos (path:tail) = if (entryName $ label rootPos) == path 
                                then focusChild rootPos tail
                                else Nothing
  where 
    focusChild :: FSZipper Full -> [FilePath] -> Maybe (FSZipper Full)
    focusChild treePos (dir:dirs) = 
      -- TODO: is join ok??
      join (fmap (\x -> focusChild x dirs) (move treePos dir))
    focusChild treePos [] = Just treePos

    move :: FSZipper Full -> String -> Maybe (FSZipper Full)
    move treePos "."  = Just treePos
    move treePos ".." = Just $ fromMaybe (root treePos) (parent treePos)
    move treePos dir  = findChild (firstChild treePos) dir

    findChild :: Maybe (FSZipper Full) -> String -> Maybe (FSZipper Full)
    findChild Nothing name = Nothing
    findChild (Just child) name = if (entryName $ label child) == name 
                                     then Just child
                                     else findChild (next child) name

mockCreateDirectory :: FilePath -> String -> FileSystemTree -> FileSystemTree
mockCreateDirectory path name (FS tree) = 
  case focusedPath of 
    Nothing -> throw $ PathDoesNotExist path
    Just parentDir -> FS $ toTree (insert newDir (children parentDir))
  where 
    focusedPath = focusPath (fromTree tree) (splitDirectories path)
    newDir = Node (Directory name) []

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

modifyFS :: (FileSystemTree -> FileSystemTree) -> MockFS s ()
modifyFS f = do
    envRef <- ask
    lift $ modifySTRef envRef (\st -> st { filesystem = f $ filesystem st })

instance FS (MockFS s) where
  createDirectory path = do
    let (dir, name) = splitFileName $ dropTrailingPathSeparator path
    modifyFS (mockCreateDirectory dir name)

  existsDirectory path = error ""

  existsFile p = return True

  listDirectory path = error ""
