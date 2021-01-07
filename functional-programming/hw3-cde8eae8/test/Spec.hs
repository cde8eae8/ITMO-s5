import qualified MockFS as M
import qualified Commands as C

import Control.Exception
import Control.Monad.ST
import Data.Maybe
import Data.Sort
import Data.Tree.Zipper
import System.FilePath
import Test.Hspec                                                                                  
import Test.Tasty                                                                                  
import Test.Tasty.Hspec

import FileManagerEnv

data ExpectedResult a = Ok (M.FileSystemTree, a)
                      | Throws 

isOk :: Either a b -> Bool
isOk (Left _) = False
isOk (Right _) = True

data TestFileSystemDescription = 
  TestFileSystemDescription 
    { descrEnv :: Env 
    , descrFS :: M.FileSystemTree
    , descrGoodDirectories :: [FilePath]
    , descrGoodFiles :: [FilePath]
    , descrNonexistingsPaths :: [FilePath]
    }

data TestHandlers a = TestHandlers 
  { handleExistingFile 
      :: FilePath 
      -> M.FileSystemTree
      -> ExpectedResult a
  , handleExistsingDirectory 
      :: FilePath 
      -> M.FileSystemTree 
      -> ExpectedResult a
  , handleNonExistingEntry 
      :: FilePath 
      -> M.FileSystemTree 
      -> ExpectedResult a
  }

tryRunMock 
  :: Env 
  -> M.FileSystemTree 
  -> M.MockFS RealWorld m 
  -> IO (Either SomeException (M.MockEnv, m))
tryRunMock env fs fsActions = do
  try (M.runMockFS env fs fsActions)

expect :: (Show a, Eq a)
  => TestFileSystemDescription 
  -> FilePath
  -> ExpectedResult a
  -> (FilePath -> M.MockFS RealWorld a) 
  -> SpecWith (Arg Expectation)
expect tfs path expected fsActions =   
  let env = descrEnv tfs
      fs = descrFS tfs in
  it path $ do
    let runResult = M.runMockFS env fs (fsActions path)
    case expected of 
      Throws -> do
        shouldThrow runResult anyException
      Ok checker -> do
        let returnValue = (\(env, val) -> (M.filesystem env, val)) <$> runResult 
        shouldReturn (fst <$> returnValue) (fst checker)
        shouldReturn (snd <$> returnValue) (snd checker)

test 
  :: (Show a, Eq a)
  => String
  -> TestFileSystemDescription 
  -> TestHandlers a
  -> (FilePath -> M.MockFS RealWorld a) 
  -> IO TestTree
test name tfs handlers fsActions = testSpec name $ do
  let fs = descrFS tfs
  describe "Nonexisting file" $ do
    mapM_ 
      (\path -> expect' (handleNonExistingEntry handlers path fs) path) 
      [ "asd"
      , "abc"
      ]
  describe "Existing directory" $ do
    mapM_ 
      (\path -> 
        expect' (handleExistsingDirectory handlers path fs) path) $
        descrGoodDirectories tfs
  describe "Existing files" $ do
    mapM_ 
      (\path -> 
        expect' (handleExistingFile handlers path fs) path)
            --(M.fileInfo $ fromJust $ M.focusPathFS fs path)) path) 
          (descrGoodFiles tfs)
  describe "Path not under root" $ do
    mapM_ 
      (\path -> expect' Throws path) 
      [ "/"
      , "/something"
      , "/home"
      ]
  where expect' = \handlers path -> expect tfs path handlers fsActions

testFileSystem :: [String] -> [String] -> IO M.FileSystemTree
testFileSystem dirs files = do
  f <- M.runMockFS (Env "" "") M.emptyFS $ do
    mapM_ (\x -> C.mkdir [x]) dirs
    mapM_ (\x -> C.mkfile [x]) files
  return $ (M.filesystem . fst) f

descr :: IO TestFileSystemDescription
descr = do
  fs <- testFileSystem dirs files
  return $ TestFileSystemDescription 
    { descrEnv = Env "" ""
    , descrFS = fs
    , descrGoodFiles = files
    , descrGoodDirectories = dirs 
    , descrNonexistingsPaths = [ ]
    }
  where 
    dirs = [ "root"
           , "root/a"
           , "root/a/b"
           , "root/a/b/c"
           , "root/b"
           , "root/b/b"
           , "root/b/b/c"
           ]
    files = [ "root/a/f1"
            , "root/a/b/f2"
            , "root/a/b/f3"
            , "root/a/b/c/f4"
            ]

pureReturn
  :: (FilePath -> M.FileSystemTree -> a) 
  -> FilePath
  -> M.FileSystemTree 
  -> ExpectedResult a
pureReturn f path fs = Ok (fs, f path fs) 

changeFS
  :: (FilePath -> M.FileSystemTree -> M.FileSystemTree) 
  -> FilePath
  -> M.FileSystemTree 
  -> ExpectedResult () 
changeFS f path fs = Ok (f path fs, ()) 

throws :: FilePath -> M.FileSystemTree -> ExpectedResult a
throws _ _ = Throws

cdHandlers :: TestHandlers ()
cdHandlers = 
  TestHandlers 
    { handleExistingFile = throws
    , handleExistsingDirectory = pureReturn (\_ _ -> ())
    , handleNonExistingEntry = throws
    }

lsHandlers :: TestHandlers [C.DirectoryEntry]
lsHandlers = 
  TestHandlers 
    { handleExistingFile = throws
    , handleExistsingDirectory = 
        pureReturn $ \path fs -> 
          let entries = snd <$> M.focusSubTreeFS 1 fs path 
              files = C.File . M.entryName <$> filter M.isFile entries 
              dirs  = C.Directory . M.entryName <$> filter M.isDirectory entries in
          sort $ files ++ dirs
    , handleNonExistingEntry = throws
    }

readHandlers :: TestHandlers String
readHandlers = 
  TestHandlers 
    { handleExistingFile = 
        pureReturn $ \path fs -> 
          M.fileContent $ M.fileInfo $ fromJust $ M.focusPathFS fs path
    , handleExistsingDirectory = throws
    , handleNonExistingEntry = throws
    }

rmFileHandlers :: TestHandlers ()
rmFileHandlers = 
  TestHandlers 
    { handleExistingFile = 
        changeFS $ \path fs -> 
          let focus = fromJust $ M.focusPathFS fs path 
              deleted = delete focus in
              M.FS $ fromJust (parent deleted)
    , handleExistsingDirectory = throws
    , handleNonExistingEntry = throws
    }

rmDirHandlers :: TestHandlers ()
rmDirHandlers = 
  TestHandlers 
    { handleExistingFile = throws
    , handleExistsingDirectory = 
        changeFS $ \path fs -> 
          let focus = fromJust $ M.focusPathFS fs path 
              deleted = delete focus in
              M.FS $ fromJust (parent deleted)
    , handleNonExistingEntry = throws
    }

infoHandlers :: TestHandlers C.Info
infoHandlers = 
  TestHandlers 
    { handleExistingFile = 
        pureReturn $ \path fs -> 
          let label = M.fileInfo $ fromJust $ M.focusPathFS fs path in
            C.F $ C.FileInfo
              { C.filePath = "." </> path
              , C.fileType = takeExtension path
              , C.filePermissions = M.filePermissions label
              , C.fileSize = M.fileSize label
              , C.modificationTime = M.modificationTime label
              }
    , handleExistsingDirectory = 
        pureReturn $ \path fs -> 
          let label = M.dirInfo $ fromJust $ M.focusPathFS fs path  
              entries = snd <$> M.focusSubTreeFS (-1) fs path :: [M.FileSystemEntry]
              files = M.fileInfoE <$> filter M.isFile entries :: [M.FileInfo]
              size = sum $ map M.fileSize files 

              lsEntries = snd <$> M.focusSubTreeFS 1 fs path 
              nFiles = length $ filter M.isFile lsEntries in
            C.D $ C.DirectoryInfo
              { C.dirPath = "." </> path
              , C.dirNFile = nFiles
              , C.dirPermissions = M.dirPermissions label
              , C.dirSize = size
              }
    , handleNonExistingEntry = throws
    }

main :: IO ()
main = do
  testDescription <- descr
  tests <- sequence 
    [ test "cd" testDescription cdHandlers (\path -> C.cd [path]) 
    , test "ls" testDescription lsHandlers (\path -> C.ls [path]) 
    , test "cat" testDescription readHandlers (\path -> C.cat [path]) 
    , test "info" testDescription infoHandlers (\path -> C.info [path]) 
    , test "rmfile" testDescription rmFileHandlers (\path -> C.rmfile [path]) 
    , test "rmdir" testDescription rmDirHandlers (\path -> C.rmdir [path]) 
    ]
  defaultMain $ testGroup "" tests 
  return ()
