{-# LANGUAGE RankNTypes #-}
module Obfuscator (obfuscate, ObfuscatorState(..), obfuscateClass) where

import Prelude hiding (lookup, init, cycle, exp)
import Expression
import qualified Data.List as L
import Data.List.NonEmpty as N (NonEmpty(..), fromList)
import Data.Map
import Data.Maybe
import Data.Tuple.Extra
import Debug.Trace
import Control.Monad.State
import Control.Monad.Identity
import System.Random

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
                    (\c -> cycle !! (fromJust (L.elemIndex c cycle) + 1)) kern
    cycle = "1Il0O"
    --newName = T.unpack $ stringRandom g ("[liI]" ++ replicate 6 "[0liI1]") name ++ show n

data ObfuscatorState = ObfuscatorState 
  { _variables :: Map String String
  , _randomName :: String
  , randomGen :: StdGen
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

randomName :: String -> State ObfuscatorState String
randomName name = do
  rand <- gets _randomName
  --let (newName, newGen) = changeName name
  let newName = changeName rand
  modify (\s -> s { _randomName = newName })
  modify $ variables (insert name newName)
  return newName

randomDefinition :: State ObfuscatorState Expression
randomDefinition = do
  newName <- randomName ""
  return $ Definition (Type "int") newName

randomAssign :: State ObfuscatorState Expression
randomAssign = do
  map <- gets _variables
  randomValue <- randomNumber (0, size map)
  let var = fromJust $ lookup (keys map !! randomValue) map
  return $ Assign (Variable $ Field $ var :| []) 
                  (Variable $ Field $ var :| [])

randomNumber :: (Int, Int) -> State ObfuscatorState Int
randomNumber (a, b) = do
  gen <- gets randomGen
  let (p, newGen) = randomR (a, b - 1) gen
  modify (\st -> st { randomGen = newGen })
  return p

choice :: [State ObfuscatorState Expression] -> State ObfuscatorState Expression
choice list = do
  randomValue <- randomNumber (0, length list)
  list !! randomValue

randomInsert :: [Expression] -> State ObfuscatorState [Expression]
randomInsert (x:xs) = do
  p <- randomNumber (0, 100)
  current <-
      if p < 90 
      then do
        randomExpression <- choice 
          [ randomDefinition
          , randomAssign
          ]
        return [x, randomExpression]
      else return [x]
  xsO <- randomInsert xs
  return (current ++ xsO)
randomInsert [] = return []

obfuscateBlock :: Block -> State ObfuscatorState Block
obfuscateBlock block = Block <$> do
  mapM obfuscate (unpack block) >>= randomInsert 
 -- state (\st -> 
 --   second (\gen -> st { randomGen = gen }) $ 
 --     runState (randomInsert l) (randomGen st))

obfuscate :: Expression -> State ObfuscatorState Expression
obfuscate (Definition typ name) = do
  newName <- randomName name
  return (Definition typ newName)

obfuscate (DefinitionInitialized typ name exp) = do
  rand <- gets _randomName
  -- let (newName, newGen) = changeName name
  let newName = changeName rand
  modify (\s -> s { _randomName = newName })
  modify $ variables (insert name newName)
  return (DefinitionInitialized typ newName exp)

obfuscate (For init cond updateExpression body) = do
  dropResult $ do
    initO <- obfuscate init
    condO <- obfuscate cond
    updateO <- obfuscate updateExpression
    bodyO <- obfuscateBlock body
    return $ For initO condO updateO bodyO

obfuscate (While cond body) = 
  dropResult $ do
    condO <- obfuscate cond
    bodyO <- obfuscateBlock body
    return $ While condO bodyO

obfuscate (If cond body) = do
  dropResult $ do
    condO <- obfuscate cond
    bodyO <- obfuscateBlock body
    return $ If condO bodyO

obfuscate (IfElse cond body elseBody) = 
  dropResult $ do
    condO <- obfuscate cond
    bodyO <- obfuscateBlock body
    elseBodyO <- obfuscateBlock elseBody
    return $ IfElse condO elseBodyO bodyO

obfuscate (Assign cond expr) = do
  condO <- obfuscate cond
  exprO <- obfuscate expr
  return $ Assign condO exprO

obfuscate (Variable var) = do
  newName <- gets $ \s name -> fromMaybe name $ lookup name (_variables s)
  Variable <$> updateName True newName var
  where 
    updateName 
      :: Bool
      -> (String -> String) 
      -> BaseOperand 
      -> State ObfuscatorState BaseOperand
    updateName v newName orig@(Field (old :| rest)) = 
      if v then return $ Field $ newName old :| rest 
           else return orig
    updateName v newName (FunctionCall (old :| rest) args) = do
      obfuscatedArgs <- mapM obfuscate args
      if v 
      then return $ FunctionCall (newName old :| rest) obfuscatedArgs
      else return $ FunctionCall (old :| rest) obfuscatedArgs
    updateName v newName (Extended l r) = do
      lO <- updateName v newName l
      rO <- updateName False newName r
      return $ Extended lO rO
    updateName _ _ (Literal t) = return $ Literal t
    updateName v newName (NewExpression t) = do
      tO <- updateName v newName t
      return $ NewExpression tO

obfuscate (OpExpression sign l r) = do
  lO <- obfuscate l
  rO <- obfuscate r
  return $ OpExpression sign lO rO

obfuscate (SingleOpExpression op) = SingleOpExpression <$> obfuscate op

obfuscate (BracketExpression expr) = do
  e <- obfuscate expr
  return $ BracketExpression e

dropResult :: State ObfuscatorState Expression -> State ObfuscatorState Expression 
dropResult st = do
  init <- get 
  e <- st
  modify (const init)
  return e

obfuscateFunction :: Function -> Function 
obfuscateFunction (Function attrs typ name args body) = res 
  where 
   res = 
      evalState (do
        newArgs <- mapM (\(t, n) -> (\s -> (t, s)) <$> randomName n) args
        newBlock <- mapM obfuscate (unpack body)
        return $ Function attrs typ name newArgs (Block newBlock))
        ObfuscatorState { _variables = empty 
                        , _randomName = "_O" 
                        , randomGen = mkStdGen 0 }

obfuscateClass :: Class -> Class
obfuscateClass (Class attrs name functions) = Class attrs name (obfuscateFunction <$> functions)

