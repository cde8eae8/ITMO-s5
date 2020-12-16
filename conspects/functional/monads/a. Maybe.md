> Semigroup a => Semigroup Maybe a
Semigroup a => Monoid Maybe a
Foldable
Functor
Applicative
Alternative
Traversable
Monad

### Semigroup
```haskell
instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)
```

### Monoid
Semigroup + mempty = Nothing

### Foldable
foldMap = mempty | Nothing
foldl = f z x | Nothing

### Applicative
pure x = Just x
<*> как fmap + достать из контекста функцию

### Monad
Nothing -> Nothing
Just x -> Just f x

### Traversable
#todo 

### Alternative
Вернет первый Just среди аргументов