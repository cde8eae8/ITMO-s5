{-# LANGUAGE LambdaCase #-}
module Parser 
  ( Parser(..)
  , brackets
  , element
  , eof
  , ok
  , parseInt
  , parseListOfLists
  , satisfy
  , stream
  ) where

import Control.Applicative
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char
import Control.Monad

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap f (Parser run) = 
      Parser $ run >=> (\(val, st) -> Just (f val, st))

instance Applicative (Parser s) where
    pure a = Parser $ \s -> return (a, s)

    (Parser first) <*> (Parser second) = 
      Parser $ \s -> do
        (func, st) <- first s 
        (val, st2) <- second st
        return (func val, st2)

instance Monad (Parser s) where
  return = pure
  
  (Parser run) >>= func = 
    Parser $ \s -> do
      (val, s2) <- run s
      runParser (func val) s2

instance Alternative (Parser s) where
  empty = Parser $ \s -> Nothing

  l <|> r = 
    Parser $ \s -> case (runParser l s) of 
                     Nothing -> case (runParser r s) of
                                        Nothing -> Nothing
                                        v -> v
                     v -> v

ok :: Parser s ()
ok = return ()

eof :: Parser s ()
eof = Parser $ \s -> case s of
                  [] -> Just ((), s)
                  _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy pred = Parser $ \case
                          [] -> Nothing
                          (x:xs) -> 
                            if pred x 
                            then Just (x, xs) 
                            else Nothing

element :: (Eq s) => s -> Parser s s
element c = satisfy (c ==)

stream :: (Eq s) => [s] -> Parser s [s]
stream str = Parser $ \s -> 
  if str `isPrefixOf` s 
  then Just (str, drop (length str) s) 
  else Nothing

oneOf :: (Eq s) => [s] -> Parser s s 
oneOf set = satisfy (\x -> isJust $ find (x ==) set)

digit :: Parser Char Int
digit = fmap digitToInt (oneOf "1234567890") 

skipSpace :: Parser Char ()
skipSpace = satisfy isSpace >> ok

skipSpaces :: Parser Char ()
skipSpaces = many skipSpace >> ok

brackets :: Parser Char ()
brackets = bracketOrEmpty >> eof
  where open = element '('
        close = element ')'
        bracketOrEmpty = 
          (do
            open 
            bracketOrEmpty 
            close 
            bracketOrEmpty) <|> ok

parseInt :: Parser Char Int
parseInt = do
  s <- sign 
  digits <- some digit
  return $ s * foldl (\acc x -> acc * 10 + x) 0 digits
  where sign = (const 1)    <$> (element '+') 
           <|> (const (-1)) <$> (element '-') 
           <|> (const 1)    <$> ok

parseList :: Parser Char [Int]
parseList = do
  length <- parseInt
  replicateM length (do
                      skipSpaces
                      (element ',')
                      skipSpaces
                      parseInt)
  

parseListOfLists :: Parser Char [[Int]]
parseListOfLists = do
  skipSpaces
  first <- parseList 
  tail <- many (do
              skipSpaces
              element ','
              skipSpaces
              parseList)
  return $ first : tail
