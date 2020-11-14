module Monoids where

import Data.Foldable
import Data.Maybe

data NonEmpty a = a :| [a] deriving (Show, Eq)
data ThisOrThat a b = This a | That b | Both a b

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat list = fromMaybe [] $ fold list

instance Semigroup (NonEmpty a) where
  (a :| as) <> (b :| bs) = a :| (as ++ [b] ++ bs)

instance Semigroup (ThisOrThat a b) where
  (Both a b) <> _  = Both a b
  _ <> (Both a b)  = Both a b
  This a <> That b = Both a b
  This a <> This _ = This a
  That b <> This a = Both a b
  That b <> That _ = That b

