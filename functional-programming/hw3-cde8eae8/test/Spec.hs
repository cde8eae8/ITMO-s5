import MockFS 
import Data.Tree
import Commands
import FileManagerEnv

main :: IO ()
main = do
  let fs = FS (Node (Directory "/") [])
  let fs2 = mockCreateDirectory "/" "something" fs
  --let fs3 = createDirectory "/something/" "a" fs2
  --let fs4 = createDirectory "/some/" "a" fs3
  --let fs2 = createDirectory "/" "something2" fs2
  (st, res) <- runMockFS (Env "/" "") (FS $ Node (Directory "/") []) $ do
    let dirs = [ "/a"
               , "/a/b"
               , "/a/b/c"
               , "/b"
               , "/b/b"
               , "/b/b/c"
               , "/b/"
               ]
    mapM_ (\x -> mkdir [x]) dirs
  print $ st
