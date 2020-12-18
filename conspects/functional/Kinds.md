#colloc2 
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

Короче. Теперь ты можешь как кайнд указать тип и его конструкторы станут типами. А до этого конструкторы это были значения.
Принято писать апостроф перед конструктором, если он на уровне типов.

Список станет списком типов, если его запромоуить. 

```haskell
data Vec :: * -> Nat -> * where
    Nil  :: Vec a 'Z
    (:>) :: a -> Vec a n -> Vec a ('S n)
```
В коде выше утверждается, что у нас есть вектор параметризованный типом и каким-то экземпляром Nat на уровне типов и вернет это все тип. 
И у него есть 2 обычных конструктора, первый просто пустой вектор, второй добавляет в существующий вектор новое значение.

```cpp
template <typename T> requires T = N or T = Z
class N { }

class Z { }

template <typename T, typename Nat> requires Nat = T or Nat = Z
class Vec {
	Vec(T* value, Vec T* Nat-1)

}

template <typename T> requires Nat = T or Nat = Z
class Vec<T, 0> {
	Vec();
}

```

# Списки
```haskell
data HList :: [*] -> * where
    HNil :: HList '[]
    (:^) :: a -> HList t -> HList (a ': t)

infixr 2 :^
```
У нас есть тип параметризованный списком типов. Пустой это просто лист параметризованный пустым списком, расширение это вернуть список параметризованный большим листом типов и сохранить значение.

Кажется тут везде юзается GADT.

# Литералы типов
Ну, они есть

# Type-level functions

```haskell
newtype Foo bar = MkFoo { unFoo :: bar }

MkFoo :: bar -> Foo bar    (term level)
Foo :: * -> *              (type level)
```

```haskell
data Foo a where
  Foo :: Int -> Foo Int
  Bar :: Char -> Foo Double
  
Foo :: Int -> Foo Int      (term level)
Bar :: Char -> Foo Double  (term level)
Foo :: * -> *              (type level)
```

Хотим 
```haskell
Foo :: Int -> Foo Int      (term level)
Bar :: Char -> Foo Double  (term level)
Foo :: * -> *              (type level)
```

Получаем 
```haskell
-- close - может расширяться только после where
type family Foo bar :: * where
  Foo Char = Double
  Foo b = b

-- open - может расширяться везде
type family Foo bar :: *
type instance Foo Char = Double
type instance Foo Int = Int
```

По идее это еще одно обощение GADT, просто расширяемое везде?

Можно привязать type к тайпклассу
```haskell
class Foo p where
  type AType p :: *
  data BType p :: *

  make :: AType p -> BType p

instance Foo Int where
  type AType Int = Int
  data BType Int = B Integer
  
  make = B . toInteger
```

# Неинъективность

При использовании type family если явно не указать что она инъективна, то мы проиграем с использованием этой функции и не можем ее проинстанциировать
```haskell
type family Foo bar :: * where
  Foo Char = Double
  Foo b = b

show :: Foo a -> String
show f = "123"
```

Решение: инъективность
```haskell
{-# LANGUAGE TypeFamilyDependencies #-}

type family Foo a = r | r -> a where
  Foo Char = Double
  Foo Int = Int
```

Решение: использовать data family
Для каждого нового аргумента содается новый тип, поэтому по определению инъективны

Решение: юзать 
```haskell
type family Foo bar :: *

newtype FooWrapped bar = Foo { unFoo :: Foo bar }
```
Как-то это работает...

# Free monads
```haskell
data Free f a = Pure a | Free (f (Free f a))

data Action a =
  PrintLn String a | ReadInt (Int -> a)
  

justPrint :: Free Action Int
justPrint = Free $ PrintLn "poid" (Pure 5)

readIncPrint :: Free Action ()
readIncPrint =
  Free $ ReadInt $ \i ->
    Free $ PrintLn (show $ i + 1) (Pure ())
	
runAction :: Free Action a -> IO a
runAction (Pure x) = pure x
runAction (Free (PrintLn s cont)) =
  putStrLn s *> runAction cont
runAction (Free (ReadInt cont)) = do
  i <- fmap read getLine
  runAction (cont i)
  
---
instance Functor f => Monad (Free f) where
return = Pure

Pure a >>= f = f a
Free m >>= f = Free ((>>= f) <$> m)

data Action a =
  PrintLn String a | ReadInt (Int -> a)
  deriving Functor

printLn :: String -> Free Action ()
printLn s = Free $ PrintLn s (Pure ())

readInt :: Free Action Int
readInt = Free $ ReadInt Pure

readIncPrint :: Free Action ()
readIncPrint = do
  i <- readInt
  printLn $ show (i + 1)

```




- \>>= is O(d) for d being a depth of computation
- Interpreting step-by-step is much worse than compilation
- You can not formalize something with two continuations, like concurrently

