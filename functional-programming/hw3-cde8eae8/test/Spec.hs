import MockFS 
import Data.Tree
import Commands
import FileManagerEnv

main :: IO ()
main = do
  --let fs = emptyFS
  --let fs2 = mockCreateDirectory "/" "something" fs
  --let fs3 = createDirectory "/something/" "a" fs2
  --let fs4 = createDirectory "/some/" "a" fs3
  --let fs2 = createDirectory "/" "something2" fs2
  (st, res) <- runMockFS (Env "/" "") emptyFS $ do
    let dirs = [ "/a"
               , "/a/b"
               , "/a/b/c"
               , "/b"
               , "/b/b"
               , "/b/b/c"
               ]

    let files = [ "/a/f1"
                , "/a/b/f2"
                , "/a/b/f3"
                , "/a/b/c/f4"
                ]
    let rmFiles = [ "/a/f1"
                  , "/a/b/f2"
                  ]
    mapM_ (\x -> mkdirCommand [x]) dirs
    mapM_ (\x -> mkfileCommand [x]) files
    mapM_ (\x -> rmfileCommand [x]) rmFiles
    lsCommand ["/a/b"]
  print st
  print res
