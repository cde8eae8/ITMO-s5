module NonEmpty (NonEmpty(..), toList, fromList) where

import Control.Applicative

data NonEmpty a = a :| [a] deriving (Show, Eq)

toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

fromList :: [a] -> NonEmpty a
fromList [] = error "empty list"
fromList (x:xs) = x :| xs

instance Foldable NonEmpty where
  foldMap f (x :| xs) = (f x) <> (foldMap f xs)

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure f = f :| []
  (f :| fs) <*> (x :| xs) = f x :| ((fmap f xs) ++ (fs <*> (x : xs)))

instance Monad NonEmpty where
  return = pure
  (x :| xs) >>= f = h :| (t ++ (xs >>= toList . f))
    where (h :| t) = f x

instance Traversable NonEmpty where
  --traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (x :| xs) = liftA2 (:|) (f x) (traverse f xs)



