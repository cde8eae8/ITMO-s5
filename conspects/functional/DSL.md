#colloc2
# Предметно-ориентированный язык
Хотим заюзать хаскель с типовой системой чтобы писать на своем языке...
```haskell
data ArithExpr =
    AENum Int
  | AEPlus ArithExpr ArithExpr
  | AEAnd ArithExpr ArithExpr
  | AEGt ArithExpr ArithExpr

-- (23 + 12) > 170 && (35 > 47)
myExpr =
  ((AENum 23 `AEPlus` AENum 12) `AEGt` AENum 170)
  `AEAnd` (AENum 35 `AEGt` AENum 47)
  
interpret :: ArithExpr -> Either String (Either Int Bool)
interpret (AENum n) = pure $ Left n
interpret (AEAnd a b) = do
  a' <- interpretBool a
  b' <- interpretBool b
  pure $ Right (a' && b')

interpretBool :: ArithExpr -> Either String Bool
interpretBool x =
  interpret x >>= either (\x -> Left $ "Unexpected " <> show x) pure
```
Проблема: на уровне типов нет информации о том что And работает с булами, а Plus с числами.

# GADT - обощенные арифм типы данных

Можем объявлять конструкторы как функции, перечисляя типы аргументов и возвращаемое значение.
Таким образом можно наложить ограничения на аргументы и типы возвращаемых значений. В остальном это такие же конструкторы.

```haskell
data ArithExpr a where
  AENum  :: Int -> ArithExpr Int
  AEPlus :: ArithExpr Int -> ArithExpr Int -> ArithExpr Int
  AEAnd  :: ArithExpr Bool -> ArithExpr Bool -> ArithExpr Bool
  AEGt   :: ArithExpr Int -> ArithExpr Int -> ArithExpr Bool
  
interpret :: ArithExpr a -> a
interpret (AENum n) = n
interpret (AEPlus a b) = interpret a + interpret b
interpret (AEAnd a b) = interpret a && interpret b
interpret (AEGt a b) = interpret a > interpret b

instance Show (ArithExpr a) where
  show (AEBool b) = show b
  show (AEGt a b) = show a <> " > " <> show b
  show (AEAnd a b) = show a <> " && " <> show b
  show (AENum a) = show a
  show (AEPlus a b) = show a <> " + " <> show b
```

# Экзистенциальные типы
Например, мы хотим парсер для штуки выше. 
```haskell
parser :: String -> Maybe ArithExpr a
```
Означает, что для любого типа `a` мы можем вернуть `ArithExpr a`. А мы хотим сказать, что существует такой `a`.
Для этого подходят экзистенциальные типы.

```haskell
data SomeAE where
  SomeAE :: Show a => ArithExpr a -> SomeAE

parse :: String -> Maybe SomeAE
parse "1" = Just (SomeAE $ AENum 1)
parse "1+2" = Just $ SomeAE $
  AENum 1 `AEPlus` AENum 2
parse _ = Nothing

interpretShow :: SomeAE -> String
interpretShow (SomeAE expr) =
    show (interpret expr)
```

Это внутри работает как 
```haskell
{-# LANGUAGE ExistentialQuantification #-}
-- расширение просто позволяет написать forall тут

data SomeAE = forall a. Show a => SomeAE (ArithExpr a)
```

# А теперь обратно
```haskell
data SomeAE where
  SomeAE :: (Typeable a, Show a) => ArithExpr a -> SomeAE
```

Можем получать в рантайме представление типа - что-то в духе `instanceof`.
Есть метод `eqT`, который принимает два `Typeable` и возвращает `Just Refl` если они равны и `Nothing` иначе.
```haskell
-- | The class 'Typeable' allows
-- a concrete representation of
-- a type to be calculated.
class Typeable (a :: k)

-- | Propositional equality.
-- If @a :~: b@ is inhabited by some
-- terminating value, then the type @a@
-- is the same as the type @b@.
data a :~: b where
  Refl :: a :~: a

-- | Extract a witness of equality
-- of two types
eqT
  :: forall a b. (Typeable a, Typeable b)
  => Maybe (a :~: b)
```

#todo что делают TypeApplications и ScopedTypeVariables??

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

parseInt
  :: String -> Maybe (ArithExpr Int)
parseInt s = parse s >>=
  \(SomeAE (expr :: ArithExpr t)) ->
    -- do для Maybe монады
    do
	  -- проверяем типы на равенство, иначе Nothing
      Refl <- eqT @t @Int   		-- собака позволяет взять тип переменной
      pure expr
```

Суть процесса в общем случае это получить экзистенциальную хрень, а потом вот так развернуть в обычный тип.

# forall
```haskell
length :: [a] -> Int
length :: forall a . [a] -> Int -- для любого a есть функция
```

Тут не сойдется по типам - тип `a` это не тип `b`.
```haskell
applyToTuple :: ([a] -> Int) -> ([b], [c]) -> (Int, Int)
applyToTuple f (x, y) = (f x, f y)
```

А тут мы можем применить функцию к паре листов, если ее можно применить к любому листу.
```haskell
{-# LANGUAGE RankNTypes #-}

applyToTuple :: (forall a. [a] -> Int) -> ([b], [c]) -> (Int, Int)
applyToTuple f (x, y) = (f x, f y)

applyToTuple length ("hello", [1,2,3])
```

#todo http://slides.com/fp-ctd/lecture-11/fullscreen#/13/0/3
#todo ST Monad and RankNTypes

# forall vs ~~exists~~
exists:
```haskell
data Action a = 
 forall m . (MonadReader Ctx m, MonadCatch m)
 	=> Action (m a)
```
exists x2:
```haskell
data Action a where 
  Action :: forall m . (MonadReader Ctx m, MonadCatch m)
   	=> m a -> Action a
```

forall:
```haskell
data Ctx = Ctx { modulus :: Int }

newtype Action a = Action
  { runAction :: forall m . (MonadReader Ctx m, MonadCatch m)
      => m a }
```

# ScopedTypeVariables
```haskell
calc :: Num a => a -> a -> a
calc a b = a + f b
  where
    f :: a -> a  		-- это не тот же a что выше
    f = (+ 10)
```

А если добаить ScopedTypeVariables, то это будет один тип

Часто используется вместе с TypeApplications - явно задать типовые параметры функции.

# final tagless
```haskell
class ArithExpr expr where
  aeNum  :: Int -> expr Int
  aePlus :: expr Int -> expr Int -> expr Int
  aeAnd  :: expr Bool -> expr Bool -> expr Bool
  aeGt   :: expr Int -> expr Int -> expr Bool
 
myExpr :: ArithExpr expr => expr Bool
myExpr = ((aeNum 23 `aePlus` aeNum 12) `aeGt` aeNum 170)
           `aeAnd` (aeNum 35 `aeGt` aeNum 47)
```
Это не требует подключения расширений.

```haskell
newtype Interpret a =
  Interpret { interpret :: a }

-- interpret
instance ArithExpr Interpret where
  aeNum = Interpret
  aePlus a b = Interpret $
    interpret a + interpret b
  aeAnd a b = Interpret $
    interpret a && interpret b
  aeGt a b = Interpret $
    interpret a > interpret b

-- to string
newtype ToS a = ToS {toString :: String}
  deriving (Show, Semigroup)

castTS :: ToS a -> ToS b
castTS (ToS s) = ToS s

instance ArithExpr ToS where
  aeNum = ToS . show
  aePlus a b = a <> (ToS " + ") <> b
  aeAnd a b = a <> (ToS " && ") <> b
  aeGt a b =
    castTS a <> (ToS " > ") <> castTS b
```
 Чем-то напоминает индукцию по структуре с МЛ/ТТ
 
 # Сравнение
 final tagless также работает если мы хотим тайпкласс, который просто представляет какие-то эффекты, то есть это более общая штука. #todo - чего?
 
 Также хаскель для final tagless может оптимизировать код, а с GADT нет. #todo - почему?
 
 
#todo vs Free Monads see https://serokell.io/blog/tagless-final
 # DSL для языков программирования
 
 На хаскеле удобно писать интерпретаторы для языков программирования.
 