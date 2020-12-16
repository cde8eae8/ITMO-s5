```haskell
throwIO :: Exception e => e -> IO a
```

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

```haskell
import           Control.Exception (ArithException (..), catch, throwIO)
import           Control.Monad     (when)

readAndDivide :: IO Int
readAndDivide = do
    x <- readLn
    y <- readLn
    when (y == 0) $ throwIO DivideByZero
    return $ x `div` y

safeReadAndDivide :: IO Int
safeReadAndDivide = readAndDivide `catch` \DivideByZero -> return (-1)
```

IO может бросать любые исключения, но явно это не прописывается

Можно делать свои исключение реализуя тайпкласс Exception

---

```haskell
try     :: Exception e => IO a -> IO (Either e a)
tryJust :: Exception e => (e -> Maybe b) -> IO a -> IO (Either b a)

finally :: IO a	 -- computation to run first
        -> IO b	 -- computation to run afterward (even if an exception was raised)
        -> IO a

-- | Like 'finally', but only performs the final action 
-- if there was an exception raised by the computation.
onException :: IO a -> IO b -> IO a
```

RAII:
```haskell
bracket :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
```