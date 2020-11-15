module Tree 
  ( Tree(..)
  ) where

data Tree a = Branch (Tree a) (Tree a)
            | Leaf a deriving (Eq, Show)

instance Foldable Tree where
  foldMap f (Leaf a) = f a
  foldMap f (Branch a b) = (foldMap f a) <> (foldMap f b)

instance Functor Tree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf

  (Branch l r) <*> t = Branch (l <*> t) (r <*> t)
  (Leaf f) <*> t = fmap f t

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Branch a b) = Branch <$> traverse f a <*> traverse f b

