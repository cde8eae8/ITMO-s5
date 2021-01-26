module Main where

import Prelude hiding (null)
import Grammar
import Lexer
import Obfuscator
import Expression
import Control.Monad.State
import Data.List
import Data.Map 
import System.Random

main :: IO ()
main = do
  s <- getContents
  let toks = alexScanTokens s
  --let exprs = 
  --      evalState (mapM obfuscate (unpack $ parse toks))
  --        ObfuscatorState { _variables = empty 
  --                        , _randomName = "_O" 
  --                        , randomGen = mkStdGen 0 }
  let (imports, clazz) = parse toks
  let obfuscated = obfuscateClass clazz
  putStrLn $ intercalate "\n" (show <$> imports)
  putStrLn $ show obfuscated
  -- putStrLn $ intercalate "\n" (fmap show exprs)


