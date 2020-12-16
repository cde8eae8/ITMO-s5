# DList
 когда нам надо соединить много списков, мы работаем за квадрат длины. `[n] ++ [m] ++ [k]` работает как `[n] ++ ([m] ++ [k])` и работает за $n + m$. `([n] ++ [m]) ++ [k]` работает за $n + (n + m)$. Чтобы не проигрывать с этим можно использовать `DList`
```haskell	
-- накапливает \x -> (x1 ++ (x2 ++ (x3 ++ (x4 ++ (... ++ (xn ++ x)))))
newtype DList a = DL { unDL :: [a] -> [a] }

fromList :: [a] -> DList a
fromList l = DL (l++)

toList :: DList a -> [a] 
toList (DL lf) = lf []

-- запихнем в самый низ левой части всю правую 
-- с возможностью засунуть еще глубже новый аргумент x
append :: DList a -> DList a -> DList a
append (DList l) (DList r) = DList $ \x -> l r x
```
Можно сделать моноид
Но при этом вызовы функций тоже не бесплатные

# Data.Sequence
добавление в концы за константу, конкатенация за лог

# Strictness
Сохранение thunk на куче занимает время и память, но есть случаи когда нам достаточно строго вычислить значение.
Также в foldr например у нас формируется огромная цепочка невычисленных значений, что гробит память.
### seq
```haskell
seq :: a -> b -> b  -- just a model, not a real implementation
_|_ `seq` _ = _|_
_   `seq` b = b
```
Доводит первый аргумент до головной нормальной формы, возвращая второй (другой способ - явный паттерн матчинг)
> **Для `newtype` новый тип не существует в рантайме и в `seq` вычислится WHNF аргумента**

### foldl' vs foldl vs foldr
Проблема последних в том что они лениво накапливают вычисления внутри себя. Первая через seq форсит строгое применение функции.
Но даже foldl' не срабатывает, если недостаточно привести аргумент к слабой головной нормальной форме, например если мы собираем лист пар - для них СГНФ это сама пара, и результат суммы будет лениво накапливаться в элементах пары.
Для списка СГНФ это первый элемент

### deepseq & NFData
`Control.DeepSeq`
Вычисляет до НФ
```haskell
class NFData a where  -- Normal Form Data
    rnf :: a -> ()
    rnf a = a `seq` ()
```
```haskell
instance NFData a => NFData (Maybe a) where
    rnf Nothing  = ()
    rnf (Just x) = rnf x

instance NFData a => NFData [a] where
    rnf []     = ()
    rnf (x:xs) = rnf x `seq` rnf xs
```

Заметь как выглядит для листа - `seq` тут чтобы зафорсить конструктор `()` и вызывать `rnf x`
deepseq может дерайвится компилятором

### -XBangPatterns
! в любом (?) месте паттерн матчинга позволяет вычислить до СГНФ этот аргумент 
Также есть функции `$!` и `$!!`, это строгое и дипстрогое применение функции. (Посмотри презентацию)

### ~Ленивые паттерны
См. презу
Позволяет пропустить паттерн матчинг, компилятор просто поверит что это абсолютный матч и не станет проверять и приводить к СГНФ

### Strict Haskell
```haskell
-- поля структуры автоматически всегда вычисляются до WHNF
{-# LANGUAGE StrictData #-} 
-- делает все вычисления в файле строгими
{-# LANGUAGE Strict #-}  -- but nobody uses this
```

Строгость полезна при 
- медленных вычислениях/переполнениях стека
- операциях над числами
- заредьюсить рекурсивные вызовы или композицию через `$!`
- поля структур, чтобы избежать ликов памяти

# Deforesation
Хотим по правилам убрать лишние структуры, например сложить несколько мап в один фор через использование case
#todo

# Stream
```haskell
newtype List a = List ([a] -> Maybe (a, [a]))

map1 :: (a -> b) -> List a -> List b
map1 g (List f) = List h
  where
    h s' = case f s' of
      Nothing       -> Nothing
      Just (x, s'') -> Just (g x, s'')
```

```haskell
stream :: forall a . [a] -> Stream a
stream xs = Stream next xs 
  where
    next :: [a] -> Step [a] a
    next []     = Done
    next (x:xs) = Yield x xs
	
unstream :: forall a . Stream a -> [a]
unstream (Stream next s0) = go s0 
  where
    go s = case next s of 
             Done       -> []
             Skip s'    -> go s'
             Yield a s' -> a : go s'
```
Плюсы: мы не меняем списки, только штуки для фильтрации
Реализуем `filter'` как (`stream` + накидывание `Skip` + `unstream`), мап как это же + применение функции, фолдр тоже как-то. Добавим реврайт правило `stream . unstream s = s`. 
В итоге у нас соптимизировались в ноль эти операции, а операции над листом стали операцией над функцией и итерируемся мы 2 раза, а не n.

```haskell
{-# RULES "stream/unstream" 
    forall (s :: Stream a) . stream (unstream s) = s 
  #-}
```

# Mutable Objects
Для `IOArray` мы должны использовать `IO`, хотя общения с внешним миром нет. А в любом месте получить `IO` или избавиться от него сложна.

Есть `ST` монада - _строгая_ *__чистая__* монада для мутабельных объектов.

```haskell
-- import Control.Monad.ST
data ST s a  -- The strict state-transformer monad

runState :: State s a -> s -> (a, s)  -- use evalState with state to get result
runST    :: (forall s. ST s a) -> a   -- forall trick
```

```haskell
data STRef s a  -- a mutable variable

newSTRef    :: a -> ST s (STRef s a) 
readSTRef   :: STRef s a -> ST s a
writeSTRef  :: STRef s a -> a -> ST s ()
modifySTRef :: STRef s a -> (a -> a) -> ST s () 

sumST :: Num a => [a] -> a
sumST xs = runST $ do
    n <- newSTRef 0
    for_ xs $ \x ->
        modifySTRef n (+x)
    readSTRef n
```

### Mutable array in ST
```
class Monad m => MArray a e m where  -- type class for all arrays

-- import Data.Array.ST
data STArray s i e :: * -> * -> * -> *  -- Mutable, boxed, non-strict arrays
-- s: the state variable argument for the ST type
-- i: the index type of the array (should be an instance of Ix), usually Int
-- e: the element type of the array.

-- храним не ссылки а реальные байты
data STUArray s i e  -- A mutable array with unboxed elements 
                     -- (Int, Double, Bool, etc.)

-- отрезок индексов, начальные значения
newArray   :: Ix i => (i, i) -> e -> m (a i e) 
-- получить значение
readArray  :: (MArray a e m, Ix i) => a i e -> i -> m e
-- записать значение
writeArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
```

### Вектор - темная магия с расширяемостью
#todo ST вектор

### Измерение времени
Пакет Criterion
+ Ссылочки
