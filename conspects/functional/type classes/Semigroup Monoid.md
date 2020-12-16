### Semigroup - есть ассоциативная операция свертки
```hs
-- MCD: <>
-- [Associativity] x <> (y <> z) = (x <> y) <> z

class Semigroup where
	(<>) :: a -> a -> a
	sconcat :: NonEmpty a -> a
	stimes :: Integral b => b -> a -> a
```

### Monoid - есть ассоциативная операция свертки с нулем
```haskell
-- MCD: mempty
-- Right identity: x <> mempty = x
-- Left identity:  mempty <> x = x
-- Associativity:  x <> (y <> z) = (x <> y) <> z (Semigroup law)
-- Concatenation:  mconcat = foldr (<>) mempty

class Semigroup a => Monoid a where
	mempty :: a 
	mappend :: a -> a -> a 
	mconcat :: [a] -> a 
```

---

Foldable - можно свернуть структуру в значение
```haskell
-- MCD: foldMap | foldr
-- t :: (* -> *)

class Foldable t where 
	foldMap :: Monoid m => (a -> m) -> t a -> m 
	foldr :: (a -> b -> b) -> b -> t a -> b 
	foldl :: (b -> a -> b) -> b -> t a -> b 
	etc.
```

### Traversable - ???
#todo
```haskell
-- MCD: traverse | sequenceA
-- t :: (* -> *)

class (Functor t, Foldable t) => Traversable t where 
	traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 
	sequenceA :: Applicative f => t (f a) -> f (t a) 
	mapM :: Monad m => (a -> m b) -> t a -> m (t b) 
	sequence :: Monad m => t (m a) -> m (t a) 
```

### Alternative - monoid for Applicative
По смыслу объединяет результаты Applicatives
```haskell
-- MCD: <|>, empty
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
	some :: f a -> f [a] -- one or more
    many :: f a -> f [a] -- zero or more
```

---

### Functor - применить функцию сквозь контекст
```haskell
-- MCD: fmap
-- Identity    fmap id == id
-- Composition fmap (f . g) == fmap f . fmap g

class Functor f where 
	-- применить ф-цию сквозь контекст/поднять до контекста
	fmap :: (a -> b) -> f a -> f b 
	(<$) :: a -> f b -> f a infixl 4
-- также где-то <$> аналог fmap в мире операторов
```

### Applicative - применить функцию в контексте сквозь контекст
```haskell
-- Identity       pure id <*> v = v
-- Composition    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- Homomorphism   pure f <*> pure x = pure (f x)
-- Interchange    u <*> pure y = pure ($ y) <*> u
-- MCD: pure, ((<*>) | liftA2)

class Functor f => Applicative f where 
	pure :: a -> f a 
	(<*>) :: f (a -> b) -> f a -> f b infixl 4 
	(*>) :: f a -> f b -> f b infixl 4 
	(<*) :: f a -> f b -> f a infixl 4 
-- также где-то liftA2
```

---

### Monad - цепочка вычислений, где следующее может опираться на предыдущее
```haskell
-- Left identity     return a >>= k = k a
-- Right identity    m >>= return = m
-- Associativity     m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- MCD: >>=

class Applicative m => Monad m where 
	(>>=) :: forall a b. m a -> (a -> m b) -> m b infixl 1 
	(>>) :: forall a b. m a -> m b -> m b infixl 1 
	return :: a -> m a 
```

Monads composition
```haskell
-- (f >=> g) >=> h ≡ f >=> (g >=> h)    -- associativity
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

```haskell
-- 'guard' is a polymorphic function but for lists looks like this:
guard :: Bool -> [()]
guard True  = [()]
guard False = []
```

Join monads
```haskell
join :: Monad m => m (m a) -> m a
```