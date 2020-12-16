module Tree (Tree(..), isEmpty, size, find, contains, Tree.insert, erase, Tree.fromList) where

import Data.Maybe 
import Data.List.NonEmpty as NN
import Prelude hiding (head, length, tail, insert)

data Tree a =  Leaf | Node (NonEmpty a) (Tree a) (Tree a) deriving Show

instance (Ord a) => Eq (Tree a) where
  (==) Leaf Leaf = True
  (==) (Node leftXs leftL leftR) (Node rightXs rightL rightR) = 
    leftXs == rightXs && leftL == rightL && leftR == rightR
  (==) _ _ = False

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: (Ord a) => Tree a -> Int
size Leaf          = 0
size (Node xs l r) = (size l) + (size r) + (length xs)

find :: (Ord a) => Tree a -> a -> Maybe (NonEmpty a)
find Leaf _          = Nothing 
find (Node xs l r) elem 
  | elem < key  = find l elem
  | elem > key  = find r elem
  | elem == key = Just xs
  where key = head xs

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf elem          = Node (elem :| []) Leaf Leaf
insert (Node xs l r) elem
  | elem < key  = Node xs (Tree.insert l elem) r
  | elem > key  = Node xs l (Tree.insert r elem)
  | elem == key = Node (elem :| NN.toList xs) l r 
  where key = head xs

contains :: (Ord a) => a -> Tree a -> Bool
contains elem t = isJust $ find t elem 

merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge Leaf Leaf                         = Leaf
merge l@(Node _ _ _) Leaf               = l
merge Leaf r@(Node _ _ _)               = r
merge l@(Node lxs ll lr) r@(Node _ _ _) = Node lxs ll (merge lr r)

erase :: (Ord a) => Tree a -> a -> Tree a
erase Leaf elem          = Leaf
erase (Node xs l r) elem
  | elem < key  = Node xs (erase l elem) r
  | elem > key  = Node xs l (erase r elem)
  | elem == key = merge l r
  where key = head xs

fromList :: (Ord a) => [a] -> Tree a
fromList list = foldl Tree.insert Leaf list
