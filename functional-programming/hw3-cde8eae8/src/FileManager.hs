{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module FileManager (FileManager, runFileManager) where
import FS
import Data.IORef
import Data.Maybe
import FileManagerEnv
import Control.Monad.Except
import Control.Monad.Reader
import System.Directory as Dir
import qualified System.IO.Strict as SIO

type FileManager = ReaderT (IORef Env) IO

runFileManager :: FileManager m -> Env -> IO (Env, m)
runFileManager fm env = do
  envRef <- newIORef env
  res <- runReaderT fm envRef
  env <- readIORef envRef
  return (env, res)

instance ModifyEnv FileManager where
  modifyEnv f = do
    ref <- ask
    lift $ modifyIORef ref f
    newRef <- ask
    liftIO $ readIORef newRef

instance FS FileManager where
  createDirectory path = lift $ Dir.createDirectory path

  createFile path = lift $ Prelude.writeFile path ""

  removeDirectory path = lift $ Dir.removeDirectory path

  removeFile path = lift $ Dir.removeFile path

  existsDirectory path = lift $ doesDirectoryExist path

  existsFile path = lift $ doesFileExist path

  listDirectory path = lift $ Dir.listDirectory path

  readFile path = do
    liftIO $ SIO.run $ SIO.readFile path

  writeFile path content = do
    liftIO $ SIO.run $ SIO.writeFile path content

  getPermissions path = do
    perm <- lift $ Dir.getPermissions path
    return $ catMaybes 
      [ Read <$ (guard $ readable perm :: Maybe ())
      , Write <$ (guard $ writable perm :: Maybe ())
      , Execute <$ (guard $ executable perm :: Maybe ())
      ]
  
  getFileSize path = lift $ Dir.getFileSize path

  getModificationTime path = lift $ Dir.getModificationTime path
