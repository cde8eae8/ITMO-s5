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

# Datatype promotion
Векторы фиксированной длины:
```haskell
data Z
data S n -- кодирование натуральных чисел

-- вектор с явным указанием длины
data Vec :: * -> * -> * where
    Nil  :: Vec a Z
    Cons :: a -> Vec a n -> Vec a (S n)

-- вектор длины 3
v2 :: Vec Int (S (S Z))
v2 = 1 `Cons` (2 `Cons` Nil)
```
Но мы можем написать 
```haskell
v3 :: Vec Int Char
v3 = ??
```

### -XDataKinds
Можно использовать типы в качестве кайндов
```haskell
{-# LANGUAGE DataKinds #-}

data Nat = Z | S Nat

data Vec :: * -> Nat -> * where
    Nil  :: Vec a Z
    Cons :: a -> Vec a n -> Vec a (S n)
```

Если мы хотим использовать конструктор на уровне типов, ставим перед ним `'`.
Теперь мы умеем определять функции над типами, которые возвращают типы...