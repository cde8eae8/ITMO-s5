Хотим комбинировать эффекты монад. Это в отличие от функторов и аппликативов (и остальных тайпклассов) нельзя сделать просто.

Monad | Effect
---|---
Maybe | can fail
Either | can fail with error
[] | has multiple values
Writer | monoidal accum
Reader | access to immutable context
State | mutable state
IO | IO actions

Хотим несколько эффектов в одной монаде.

#todo RWS
#todo only State lecture 7:/3/0/6

# lift
Поднимает значение в контекст монады-трансформера
```haskell
class MonadTrans t where    -- t :: (* -> *) -> * -> *
    lift :: Monad m => m a -> t m a
    
    {-# LAWS

        1. lift . return  ≡ return
        2. lift (m >>= f) ≡ lift m >>= (lift . f)

    #-}
```

# MaybeT
```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return :: a -> MaybeT m a
    return x = MaybeT (return (Just x))

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    MaybeT action >>= f = MaybeT $ do
        result <- action
        case result of
            Nothing -> return Nothing
            Just x  -> runMaybeT (f x)
```

holds Nothing -> MaybeT Nothing
holds Just x ->  MaybeT runMaybeT (f x)
```haskell
(f x) :: a -> MaybeT m b
runMaybeT (f x) :: m (Maybe b)
```

### lift
Просунуть внутрь контекста Just. То есть получится `MaybeT m Just value`
```haskell
transformToMaybeT :: Functor m => m a -> MaybeT m a
transformToMaybeT = MaybeT . fmap Just
```

#todo Alternative example

lecture 7 39:00

# ReaderT
```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Monad (ReaderT r m) where
    return  = lift . return
    m >>= f = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (f a) r

instance MonadTrans (ReaderT r) where
    lift :: m a -> ReaderT r m a
    lift = ReaderT . const
 -- lift ma = ReaderT $ \_ -> ma
```

# StateT
```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Monad (StateT s m) where
	return a = StateT $ \s -> return (a, s)
	m >>= f = StateT $ \s -> do
		(a, new_state) <- (runStateT m) s
		runStateT (f a) new_state
```

# Все трансформеры
Base monad | Transformer | Original type | Combined type
------|---------|---------|-------------
Maybe |	MaybeT  | Maybe a |	m (Maybe a)
Either| EitherT | Either a b | m (Either a b)
Writer| WriterT | (a, w) | m (a, w)
Reader| ReaderT | r -> a | r -> m a
State |	StateT | s -> (a, s) | s -> m (a, s)
Cont  | ContT | (a -> r) -> r | (a -> m r) -> m r

NB: Нет монады для IO

# MonadIO
#todo 

# StateT + Reader example 
```haskell
foo :: Int -> StateT [Int] (Reader Int) Int
foo i = do
    baseCounter <- lift ask
    let newCounter = baseCounter + i
    put [baseCounter, newCounter]
    return newCounter
```

Но надо много lift при глубокой вложенности.

# transformers package
Только трансформеры и MonadTrans

# mtl package
transformers + Monad_Some_
Чтобы не было кучи лифтов

# MonadThrow & MonadCatch & MonadError
#todo 
