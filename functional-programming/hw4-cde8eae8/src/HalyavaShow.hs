{-# LANGUAGE MultiParamTypeClasses #-}
module HalyavaShow
  ( runShow
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Data.List

import Halyava

type PrintInner a = StateT PrintState (Writer [String]) a

newtype Print a = Print (StateT PrintState (Writer [String]) ())

data PrintState = PrintState 
  { varId :: Int 
  , indentationLevel :: Int
  }

newtype PrintVarInfo a = PrintVarInfo { name :: String }

text :: PrintInner () -> PrintInner [String]
text actions = do
  init <- get 
  let v = --intercalate "\n" $ 
        execWriter $ evalStateT actions init 
  return v

indent :: PrintInner () -> PrintInner [String]
indent actions = do
  init <- get 
  --let indentL = indentationLevel init
  let v = --intercalate "\n" $ 
        ("  " ++) <$> execWriter (evalStateT actions init)
  return v

-- Кажется оно поддерживает 
-- while { ... } + while { ... }
-- Может сделать while возвращаемый тип ()?
-- А у нормальных типов чиселки...
--
-- Может написать вместо tell tell который еще и соединит их?

showFuncCall1:: String -> Print b -> Print c
showFuncCall1 opString (Print arg) = Print $ do
    l <- text arg
    tell [ opString ++ "(" ++ concat l ++ ")" ]

showUnaryOperator :: String -> Print b -> Print c
showUnaryOperator opString (Print arg) = Print $ do
    l <- text arg
    tell [ opString ++ concat l ]

showBinaryOperator :: String -> Print a -> Print b -> Print c
showBinaryOperator opString (Print lhs) (Print rhs) = Print $ do
    l <- text lhs
    r <- text rhs
    tell [ concat l ++ opString ++ concat r ]

instance Halyava PrintVarInfo Print where
  hsDefVar (Print init) code = Print $ do
    id <- gets varId
    let name = "v" ++ show id
    let (Print codeblock) = code (HsVar (PrintVarInfo name))
    modify (\s -> s { varId = id + 1 })
    initT <- text init
    codeblockT <- text codeblock
    tell $ ( "var " ++ name ++ " = " ++ concat initT ) : codeblockT 

  hsValue (HsVar info) = Print $ do
    tell [ name info ]

  hsLiteral v = Print $ do
    tell [ show v ]

  (#) (Print prev) (Print cur) = Print $ do
    v <- text prev 
    v2 <- text cur
    tell $ v ++ v2

  hsWhile (Print cond) (Print body) = Print $ do
      state <- get
      let indentL = indentationLevel state 
      condS <- withStateT (const state { indentationLevel = 0 }) (text cond)
      bodyS <-
        withStateT (const state { indentationLevel = indentL + 1 }) (indent body)
      tell $ 
        [ "while (" ++ concat condS ++ ") {" ] ++
            bodyS ++
        [ "}" ]

  hsIf (Print cond) (Print body) = Print $ do
      state <- get
      let indentL = indentationLevel state 
      condS <- withStateT (const state { indentationLevel = 0 }) (text cond)
      bodyS <-
        withStateT (const state { indentationLevel = indentL + 1 }) (indent body)
      tell $ 
        [ "if (" ++ concat condS ++ ") {" ] ++
            bodyS ++
        [ "}" ]

  hsIfElse (Print cond) (Print body) (Print elseBody) = Print $ do
      state <- get
      let indentL = indentationLevel state 
      condS <- withStateT (const state { indentationLevel = 0 }) (text cond)
      bodyS <-
        withStateT (const state { indentationLevel = indentL + 1 }) (indent body)
      elseBodyS <-
        withStateT (const state { indentationLevel = indentL + 1 }) (indent elseBody)
      tell $ 
        [ "if (" ++ concat condS ++ ") {" ] ++
            bodyS ++
        [ "} else {" ] ++
            elseBodyS ++
        [ "}" ] 

  hsPrint (Print arg) = Print $ do
    argV <- text arg
    tell ["console.log(" ++ concat argV ++ ")"]

  (@==) = showBinaryOperator "=="
  (@/=) = showBinaryOperator "!="

  (@>) = showBinaryOperator ">"
  (@<) = showBinaryOperator "<"
  (@>=) = showBinaryOperator ">="
  (@<=) = showBinaryOperator "<="

  (@+) = showBinaryOperator "+"
  (@*) = showBinaryOperator "*"
  (@/) = showBinaryOperator "/"
  (@-) = showBinaryOperator "-"

  hsAbs = showFuncCall1 "abs"
  hsSignum = showFuncCall1 "sign"

  hsNegate = showUnaryOperator "-"

  (@&&) = showBinaryOperator "&&"
  (@||) = showBinaryOperator "||"
  (@!) = showUnaryOperator "!"

  (@++) = showBinaryOperator "+"

  hsToString (Print expr) = Print $ do
    t <- text expr
    tell [ "(" ++ concat t ++ "+ \"\")" ]

  hsIth (Print str) (Print idx) = Print $ do
    l <- text str
    i <- text idx
    tell [ concat l ++ "[" ++ concat i ++ "]"]

  (@=) (HsVar info) (Print value) = Print $ do
    r <- text value
    tell [ name info ++ " = " ++ concat r ]

runShow :: Print a -> String
runShow (Print st) = intercalate "\n" $ execWriter $ evalStateT st init
  where init = PrintState 
          { varId = 0
          , indentationLevel = 0
          }
