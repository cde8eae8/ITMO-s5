{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module FileManager (FileManager) where
import FS
import Data.IORef
import FileManagerEnv
import Control.Monad.Except
import Control.Monad.Reader
import System.Directory as Dir

type FileManager = ReaderT (IORef Env) IO

instance ModifyEnv FileManager where
  modifyEnv f = do
    ref <- ask
    lift $ modifyIORef ref f
    newRef <- ask
    liftIO $ readIORef newRef

--instance HasRootDirectory (FileManager m) String where
--  -- (String -> f String) -> Env -> f Env 
--  -- (String -> f String) -> FileManager -> f FileManager 
--  rootDirectory modifyField = do
--    env <- ask
--    modifyIORef env modifyField


--tryFS :: FS a -> StateT String IO (Either IOException a)
--tryFS = do
--  st <- lift get

instance FS FileManager where
  createDirectory path = error ""

  existsDirectory path = lift $ doesDirectoryExist path

  existsFile p = return True

  listDirectory path = lift $ Dir.listDirectory path
