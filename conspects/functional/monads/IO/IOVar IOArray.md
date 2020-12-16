```haskell
import Data.IORef (newIORef, readIORef, writeIORef)

foo :: IO ()
foo = do 
    varA <- newIORef 0 
    a0   <- readIORef varA
    writeIORef varA 1
    a1   <- readIORef varA
    print (a0, a1)
```

```haskell
import Data.Array.IO (IOArray, newArray, readArray, writeArray)

bar :: IO ()
bar = do 
    arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
    a   <- readArray arr 1
    writeArray arr 1 64
    b   <- readArray arr 1
    print (a, b)
```