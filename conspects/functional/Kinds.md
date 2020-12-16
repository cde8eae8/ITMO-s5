# Kinds
```haskell
:kind (Maybe Maybe) - ошибка, так как аргумент Maybe должен быть кайнда *

data MapTree k v

ghci> :k mapTree 
* -> * -> *

ghci> :k [Int]
* -> *

ghci> :k (->)
* -> * -> *

newtype ReaderT r (m :: * -> *) (a :: *) = 
  ReaderT {runReaderT :: r -> m a}

ghci> :kind ReaderT
ReaderT :: * -> (* -> *) -> * -> *
```

# Constraints
Констраинт это то что может быть слева от `=>`
```haskell
class Num a where ...

ghci> :kind Num
Num :: * -> Constraint
```


Можно сделать алиас для нескольких констраинтов
```haskell
{-# LANGUAGE ConstraintKinds #-}

type MyConstraints a = (Read a, Num a, Show a)

foo :: MyConstraints a => String -> a -> a
```

# -XTypeOperators
Можно использовать операторы как имена типов
```haskell
data a <$$> b = Mult a b

ghci> let x = Mult @Int 3 True
ghci> :t x 
x :: Int * Bool
```

# @Int -XTypeApplications
Позволяет явно указать каким типом инстанцировать
```haskell
{-# TypeApplications #-}
Prelude> data F a b = F a b

Prelude> :t F
F :: a -> b -> F a b

Prelude> :t F @Int
F @Int :: Int -> b -> F Int b

Prelude> :t F @Int @Bool
F @Int @Bool :: Int -> Bool -> F Int Bool

Prelude> F @Int @Bool 10 True
F 10 True
```

# 