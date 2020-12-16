C:
```c
/* clang -c simple.c -o simple.o */

int example(int a, int b)
{
  return a + b;
}
```
Haskell:
```haskell
-- ghc simple.o simple_ffi.hs -o simple_ffi

{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.Types

foreign import ccall safe "example" 
    example :: CInt -> CInt -> CInt

main = print (example 42 27)
```

---

Для нечистых сишных функций 
```haskell
foreign import ccall safe "example" 
    example :: CInt -> CInt -> IO CInt
```