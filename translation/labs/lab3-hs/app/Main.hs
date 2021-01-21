module Main where

import Prelude hiding (null)
import Grammar
import Lexer
import Obfuscator
import Expression
import Control.Monad.State
import Data.List
import Data.Map 

main :: IO ()
main = do
  s <- getContents
  let toks = alexScanTokens s
  print (toks)
  print (parse $ toks)
  print $ evalState (mapM (\s -> obfuscate s) (unpack $ parse $ toks))
          ObfuscatorState { _variables = empty 
                          , _randomName = "_O" }
  let (exprs, st) = 
        runState (mapM (\s -> obfuscate s) (unpack $ parse $ toks))
          ObfuscatorState { _variables = empty 
                          , _randomName = "_O" }
  putStrLn $ intercalate "\n" (fmap show exprs)


