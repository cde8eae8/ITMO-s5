{-# LANGUAGE InstanceSigs #-}

module FoldableTree where

import Tree

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf) = mempty
  foldMap f (Node xs l r) = (foldMap f xs) <> (foldMap f l) <> (foldMap f r)

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Leaf)        = z
  foldr f z (Node xs l r) = (foldr f (foldr f (foldr f z r) xs) l)


