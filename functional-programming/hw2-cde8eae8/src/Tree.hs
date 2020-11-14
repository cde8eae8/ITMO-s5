module Tree (Tree) where

data Tree a = Branch (Tree a) (Tree a)
            | Leaf a

instance Foldable Tree where
    foldMap f (Leaf a) = f a
    foldMap f (Branch a b) = (foldMap f a) <> (foldMap f b)

instance Functor Tree where
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
    pure = Leaf

    (Leaf f) <*> (Leaf t) = Leaf $ f t
    --(Leaf f) <*> (Branch l r) = Leaf $ f t

--instance Traversable where
    --sequence = 
