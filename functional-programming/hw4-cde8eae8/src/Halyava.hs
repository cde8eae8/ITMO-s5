{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Halyava 
  ( Halyava(..)
  , HsVar(..)
  , fibNumbers
  , runInterpret
  ) where

import Data.IORef
import Control.Monad

newtype HsVar a info = HsVar (info a)

newtype HsNumber = 
  HsNumber Double 
  deriving 
    ( Num
    , Fractional
    , Floating
    , Eq
    , Ord
    )

instance Show HsNumber where
  show (HsNumber n) = show n

class Halyava info expr | expr -> info where
  -- init with some literal
  hsDefVarL  :: (Show a) => a -> (HsVar a info -> expr ()) -> expr () 
  hsDefVarL val = hsDefVar (hsLiteral val)
  -- init with expression
  hsDefVar  :: expr a -> (HsVar a info -> expr ()) -> expr () 

  hsValue   :: HsVar a info -> expr a
  hsLiteral :: (Show a) => a -> expr a

  infixl 1 #
  (#)       :: expr () -> expr () -> expr ()
  hsWhile   :: expr Bool -> expr () -> expr ()
  hsIf      :: expr Bool -> expr () -> expr ()
  hsIfElse  :: expr Bool -> expr () -> expr () -> expr ()

  hsPrint :: (Show a) => expr a -> expr () 

  infixl 2 @= 
  (@=) :: HsVar a info -> expr a -> expr ()

  (@+) :: expr HsNumber -> expr HsNumber -> expr HsNumber
  (@-) :: expr HsNumber -> expr HsNumber -> expr HsNumber
  (@*) :: expr HsNumber -> expr HsNumber -> expr HsNumber
  (@/) :: expr HsNumber -> expr HsNumber -> expr HsNumber
  hsAbs     :: expr HsNumber -> expr HsNumber
  hsSignum  :: expr HsNumber -> expr HsNumber
  hsNegate  :: expr HsNumber -> expr HsNumber

  (@==) :: (Eq a) => expr a -> expr a -> expr Bool
  (@/=) :: (Eq a) => expr a -> expr a -> expr Bool

  (@>) :: (Ord a) => expr a -> expr a -> expr Bool
  (@<) :: (Ord a) => expr a -> expr a -> expr Bool
  (@>=) :: (Ord a) => expr a -> expr a -> expr Bool
  (@<=) :: (Ord a) => expr a -> expr a -> expr Bool

  (@&&) :: expr Bool -> expr Bool -> expr Bool
  (@||) :: expr Bool -> expr Bool -> expr Bool
  (@!)  :: expr Bool -> expr Bool 

  (@++) :: expr String -> expr String -> expr String
  hsIth :: expr String -> expr Int -> expr Char 
  hsToString :: (Show a) => expr a -> expr String

instance forall info expr . (Halyava info expr) => Num (expr HsNumber) where
  (+) = (@+)
  (*) = (@*)
  abs = hsAbs
  negate = hsNegate
  signum = hsSignum
  fromInteger a = hsLiteral (fromInteger a)

newtype InterpretVarInfo a = InterpretVarInfo { varRef :: IORef a }

newtype Interpreter a = Interpreter { interpret :: IO a }

liftInterpret 
  :: (a -> b) 
  -> (Interpreter a -> Interpreter b)
liftInterpret f a = Interpreter $ f <$> interpret a

liftInterpret2 
  :: (a -> b -> c) 
  -> (Interpreter a -> Interpreter b -> Interpreter c)
liftInterpret2 f a b = Interpreter $ liftM2 f (interpret a) (interpret b)

instance Halyava InterpretVarInfo Interpreter where
  hsDefVar init code = Interpreter $ do
    initValue <- interpret init
    ref <- newIORef initValue
    let codeblock = code (HsVar (InterpretVarInfo ref))
    interpret codeblock

  hsValue (HsVar info) = Interpreter $ readIORef $ varRef info

  hsLiteral = Interpreter . return
  prev # cur = Interpreter $ interpret prev >> interpret cur
  
  hsWhile condition body = Interpreter $ do
    cond <- interpret condition
    when cond $ do
      interpret body
      interpret $ hsWhile condition body
  
  hsIf condition trueBody = Interpreter $ do
    cond <- interpret condition
    when cond $ interpret trueBody

  hsIfElse condition trueBody falseBody = Interpreter $ do
    cond <- interpret condition
    if cond
    then interpret trueBody
    else interpret falseBody

  hsPrint expr = Interpreter $ interpret expr >>= print

  (HsVar info) @= expr = 
    Interpreter $ interpret expr >>= writeIORef (varRef info) 

  (@+) = liftInterpret2 (+) 
  (@*) = liftInterpret2 (*) 
  (@-) = liftInterpret2 (-) 
  (@/) = liftInterpret2 (/) 

  hsAbs = liftInterpret abs
  hsSignum = liftInterpret signum
  hsNegate = liftInterpret negate

  (@==) = liftInterpret2 (==)
  (@/=) = liftInterpret2 (/=)

  (@>) = liftInterpret2 (>)
  (@<) = liftInterpret2 (<)
  (@<=) = liftInterpret2 (<=)
  (@>=) = liftInterpret2 (>=)

  (@&&) = liftInterpret2 (&&) 
  (@||) = liftInterpret2 (||) 
  (@!) = liftInterpret not 

  (@++) = liftInterpret2 (++) 
  hsToString = liftInterpret show
  hsIth = liftInterpret2 (!!)

fibNumbers :: (Halyava exprInfo expr) => HsNumber -> expr ()
fibNumbers fibN = 
  -- numbers
  hsDefVarL 0 $ \f1 -> 
  hsDefVarL 1 $ \f2 ->
  hsDefVarL 0 $ \n ->
  hsWhile (hsValue n @<= hsLiteral fibN) (
    hsDefVar (hsValue f1 @+ hsValue f2) $ \next -> 
    f1 @= hsValue f2 #
    f2 @= hsValue next #
    n @= hsValue n @+ hsLiteral 1
  ) 
  #
  -- strings
  hsDefVarL "!" (\stringVariable ->
  hsPrint (hsToString (hsLiteral fibN) @++ 
    hsLiteral "-th fibonacci number is " @++ 
    hsToString (hsValue f1) @++ hsValue stringVariable))
  #
  -- bools
  hsDefVar (hsValue n @>= hsLiteral 10) (\boolVar ->
  hsDefVarL True $ \boolVar2 ->
    hsPrint 
      (hsToString (hsValue boolVar) @++ 
      hsLiteral " && " @++ 
      hsToString (hsValue boolVar2) @++
      hsLiteral " = " @++
      hsToString (hsValue boolVar @&& hsValue boolVar2)
      ))
  
runInterpret :: Interpreter a -> IO a
runInterpret (Interpreter actions) = actions

