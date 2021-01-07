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
  , example
  , fibNumbers
  , runInterpret
  --, Sh(..)
  --, toString
  ) where

import Data.IORef
import Control.Monad

newtype HsVar a info = HsVar (info a)

class (Num a, Show a) => HsNumber a where

instance HsNumber Int where

instance HsNumber Double where

class Halyava info expr | expr -> info where
  -- init with some literal
  hsDefVarL  :: (Show a) => a -> (HsVar a info -> expr ()) -> expr () 
  hsDefVarL val = hsDefVar (hsLiteral val)
  -- init with expression
  hsDefVar  :: expr a -> (HsVar a info -> expr ()) -> expr () 

  hsValue   :: HsVar a info -> expr a
  hsLiteral :: (Show a) => a -> expr a

  --hsFun2 
  --  :: (HsVar a info -> HsVar b info -> HsVar res info -> expr ()) 
  --  -> expr (a -> b -> res)
  --hsFunCall2 :: expr (a -> b -> c) -> expr a -> expr b -> expr c 

  -- ??
  --hsFun1 
  --  :: (HsVar a info -> HsVar res info -> expr ()) 
  --  -> expr (HsVar a info -> HsRet res info -> expr res)
  --hsFunCall1 
  --  :: expr (HsVar a info -> HsRet res info -> expr res) 
  --  -> expr a 
  --  -> expr res 
  -- how ??
  --hsReturn :: HsRet res info -> expr a -> expr ()

  (#)       :: expr () -> expr () -> expr ()
  hsWhile   :: expr Bool -> expr () -> expr ()
  hsIf      :: expr Bool -> expr () -> expr ()
  hsIfElse  :: expr Bool -> expr () -> expr () -> expr ()

  hsPrint :: (Show a) => expr a -> expr () 

  (@=) :: HsVar a info -> expr a -> expr ()

  -- ??
  (@+) :: (HsNumber a) => expr a -> expr a -> expr a
  (@-) :: (HsNumber a) => expr a -> expr a -> expr a
  (@*) :: (HsNumber a) => expr a -> expr a -> expr a
  (@/) :: (HsNumber a) => expr a -> expr a -> expr a
  hsAbs     :: (HsNumber a) => expr a -> expr a
  hsSignum  :: (HsNumber a) => expr a -> expr a
  hsNegate  :: (HsNumber a) => expr a -> expr a

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
  hsIth :: expr Int -> expr Char
  hsToString :: (Show a) => expr a -> expr String

instance forall info expr a . (Halyava info expr, HsNumber a) => Num (expr a) where
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
  -- FIXME: Writeonly type for return value?

  --hsFun1 
  --  :: (HsVar a info -> HsVar res info -> expr ()) 
  --  -> expr (a -> res)
  --hsFun1 body = Interpreter $ do
  --  return (\a -> body a (HsVar (InterpretVarInfo ref)))
  --hsFun2 
  --  :: (HsVar a info -> HsVar b info -> HsVar res info -> expr ()) 
  --  -> expr (a -> b -> res)
  ---- ??
  --hsFunCall1 :: expr (a -> b) -> expr a -> expr b 
  --hsFunCall1 func arg = 
  --  f <- func
  --  ref <- newIORef undefined
  --hsFunCall2 :: expr (a -> b -> c) -> expr a -> expr b -> expr c 

  --(#)       :: expr () -> expr () -> expr ()
  prev # cur = Interpreter $ interpret prev >> interpret cur
  
  --hsWhile   :: expr Bool -> expr () -> expr ()
  hsWhile condition body = Interpreter $ do
    cond <- interpret condition
    when cond $ do
      interpret body
      interpret $ hsWhile condition body
  
  --hsIf      :: expr Bool -> expr () -> expr ()
  hsIf condition trueBody = Interpreter $ do
    cond <- interpret condition
    when cond $ interpret trueBody

  --hsIfElse  :: expr Bool -> expr () -> expr () -> expr ()
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
  --lhs @/ rhs = Interpreter $ liftM2 (/) (interpret lhs) (interpret rhs)

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

--example :: forall info expr a . (Show a, Num a, Ord a, Num (expr a)) 
--        => Halyava info expr => expr a

--   TODO: fix instances
--   TODO: undefined as value of variables

fibNumbers :: (Halyava exprInfo expr) => Int -> expr ()
--fibNumbers :: Int -> Interpreter ()
fibNumbers fibN = 
  hsDefVarL (0 :: Int) $ \f1 -> 
  hsDefVarL (1 :: Int) $ \f2 ->
  hsDefVarL (0 :: Int) $ \n ->
  hsWhile (hsValue n @<= hsLiteral fibN) (
    hsDefVar (hsValue f1 @+ hsValue f2) $ \next -> 
    (f1 @= hsValue f2) #
    (f2 @= hsValue next) #
    (n @= (hsValue n @+ hsLiteral 1))
  ) #
  hsPrint 
    (hsToString (hsLiteral fibN) @++ 
    hsLiteral "-th fibonacci number is " @++ 
    hsToString (hsValue f1))
  
runInterpret :: Interpreter a -> IO a
runInterpret (Interpreter actions) = actions


--example :: (Halyava exprInfo expr) => expr ()
example :: Interpreter ()
example = 
  --hsDefVar (0 :: Int) $ \a -> a @= 0
  hsDefVarL (0 :: Int) $ \a -> 
  hsDefVar (hsValue a + 3) $ \b -> 
    hsDefVarL 0 (\d -> 
        d @= hsValue a
    ) #
    hsDefVarL (0.0 :: Double) (\d -> 
        d @= (hsValue d + hsValue d + hsLiteral 2.5)
    ) #
    hsDefVarL "123" (\d -> 
        d @= (hsValue d @++ hsValue d @++ hsLiteral "123") 
    ) #
    hsDefVarL 0 (\d -> 
        d @= hsValue a
    ) #
    --3 #
    (b @= 0) # 
    hsWhile (hsValue a @> hsValue b) (
      hsDefVarL 0 $ \c -> 
        (a @= 0) #
        (b @= 0) #
        (b @= (hsValue a + hsValue a + 10 + 20 + 30)) #
        (b @= 0) #
        (b @= 0) #
        (b @= 0) #
        (b @= 0) #
        (b @= 0) #
        hsWhile (hsValue c @> hsValue b) (
          hsDefVarL 0 (\d -> 
              d @= hsValue a
          ) #
          (c @= 1) #
          (b @= 0) #
          (b @= 0) #
          (b @= 0) #
          (b @= 0) #
          (b @= 0) #
          (b @= 0)
        )
    )
