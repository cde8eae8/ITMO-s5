{-# LANGUAGE RankNTypes #-}
module Obfuscator (obfuscate, ObfuscatorState(..)) where

import Prelude hiding (lookup)
import Expression
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List as L
import Data.Map
import Data.Maybe
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Identity
import System.Random
import Text.StringRandom
import Debug.Trace

replace :: Int -> (Char -> Char) -> String -> String
replace _ _ [] = error "idx > list length"
replace 0 f (s:ss) = f s : ss
replace idx f (s:ss) = s : replace (idx - 1) f ss

changeName :: String -> String
changeName st = next
  where 
    kern = tail st
    stLen = length kern
    idx = fromJust $ L.findIndex (/= 'O') kern
    next = '_' : if replicate stLen 'O' == kern
                  then replicate (stLen + 1) '1'
                  else replace idx 
                    (\c -> cycle !! ((fromJust $ L.elemIndex c cycle) + 1)) kern
    cycle = "1Il0O"
    --newName = T.unpack $ stringRandom g ("[liI]" ++ replicate 6 "[0liI1]") name ++ show n

data ObfuscatorState = ObfuscatorState 
  { _variables :: Map String String
  , _randomName :: String
  } deriving Show

variablesF
  :: forall f. Functor f 
  =>(Map String String -> f (Map String String)) 
  -> (ObfuscatorState -> f ObfuscatorState)
variablesF f s = (\v -> s { _variables = v}) <$> new
  where 
    old = _variables s
    new = f old

variables
  :: (Map String String -> Map String String) 
  -> (ObfuscatorState -> ObfuscatorState)
variables f s = runIdentity $ variablesF (Identity . f) s

obfuscate :: Expression -> State ObfuscatorState Expression
obfuscate (Definition typ name) = do
  rand <- gets _randomName
  --let (newName, newGen) = changeName name
  let newName = changeName rand
  modify (\s -> s { _randomName = newName })
  modify $ variables (insert name newName)
  return (Definition typ newName)

obfuscate (DefinitionInitialized typ name exp) = do
  rand <- gets _randomName
  -- let (newName, newGen) = changeName name
  let newName = changeName rand
  modify (\s -> s { _randomName = newName })
  modify $ variables (insert name newName)
  return (DefinitionInitialized typ newName exp)

obfuscate (For init cond update body) = do
  dropResult $ do
    initO <- obfuscate init
    condO <- obfuscate cond
    updateO <- obfuscate update
    bodyO <- mapM obfuscate $ unpack body
    return $ For initO condO updateO (Block bodyO)

obfuscate (While cond body) = 
  dropResult $ do
    condO <- obfuscate cond
    bodyO <- mapM obfuscate $ unpack body
    return $ While condO (Block bodyO)

obfuscate (If cond body) = do
  dropResult $ do
    condO <- obfuscate cond
    bodyO <- mapM obfuscate $ unpack body
    return $ If condO (Block bodyO)

obfuscate (IfElse cond body elseBody) = 
  dropResult $ do
    condO <- obfuscate cond
    bodyO <- mapM obfuscate $ unpack body
    elseBodyO <- mapM obfuscate $ unpack elseBody
    return $ IfElse condO (Block elseBodyO) (Block bodyO)

obfuscate (Assign cond expr) = do
  condO <- obfuscate cond
  exprO <- obfuscate expr
  return $ Assign condO exprO

obfuscate (Variable var) = do
  newName <- gets $ \s name -> fromJust $ lookup name (_variables s)
  return $ Variable (updateName newName var)
  where 
    updateName :: (String -> String) -> BaseOperand -> BaseOperand
    updateName newName (Field (old :| rest)) = 
      Field $ newName old :| rest 
    updateName newName (FunctionCall (old :| rest) args) = 
      FunctionCall (newName old :| rest) args
    updateName newName (Extended l r) = 
      Extended (updateName newName l) r

obfuscate (OpExpression sign l r) = do
  lO <- obfuscate l
  rO <- obfuscate r
  return $ OpExpression sign lO rO

dropResult :: State ObfuscatorState Expression -> State ObfuscatorState Expression 
dropResult st = do
  init <- get 
  e <- st
  modify (const init)
  return e

  




