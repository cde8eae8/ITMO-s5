```haskell
newtype Reader e a = Reader { runReader :: e -> a }
```

```haskell
instance Monad (Reader e) where
    return :: a -> Reader e a
    return a = Reader $ \_ -> a

    (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
    m >>= f = Reader $ \r -> runReader (f $ runReader m r) r
```

```haskell
ask   :: Reader e e                            -- get whole env
asks  :: (e -> a) -> Reader e a                -- get part of env
local :: (e -> b) -> Reader b a -> Reader e a  -- change env locally
```

Накапливает цепочку вычислений функциями вида `arg1 -> arg2 -> ... -> argn -> Reader e a` в 
```haskell
f :: arg1 -> ... -> Reader e a
f = do 
	...
```
А потом эта цепочка запускается через
```haskell
runReader (f arg1 ... argn) $ environment
```