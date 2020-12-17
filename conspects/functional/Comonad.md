- повторить типы и функции тайпкласса + для айдентити
# Общее представление
У нас есть контейнер, в котором мы сфокусировались на конкретном элементе.

```haskell
-- MCD: extract, duplicate | extend
class Functor w => Comonad w where
	-- получить значение на котором мы сфокусированы
    extract   :: w a -> a
	-- заменяет элементы контейнера на комонады с сфокусированным значением на этом элементе
    duplicate :: w a -> w (w a)           
	-- мапит значения в контейнере, сохраняя фокус на элемент
    extend    :: (w a -> b) -> w a -> w b 

-- «extend» in operator form with arguments flipped
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
```

Например возьмем комонаду `[9, 6, 2], 1`, тогда 
- `extract` вернет 6
- `duplicate` заменит каждый `i`-тый элемент на комонаду с фокусом на `i`-тый элемент и тем же контейнером
- `extend` просто помапит значения контейнера в новые, но при этом функция мапа может использовать индекс элемента, ==который она мапит==. То есть
```haskell
extend f с = fmap f (duplicate c)
-- 						^-- получаем лист комонад на каждый элемент
--			   ^-- мапим каждый элемент в b

duplicate c = extend id c
```

```haskell
data Identity a = Identity { runIdentity :: a }

instance Comonad Identity where
    extract   = runIdentity
    duplicate = Identity
```

> **==список/Maybe не являются комонадами, так как могут быть пустыми==**

### Сравнение с монадой
return <-> extract
\>>= <-> =>>
join <-> duplicate

# Zipper
Фокус на элемент + можно шагать к другим элементам.
Ну и они комонады, потому что могут фокусировать элементы + мы можем посмещаться на другие элементы и применить мап, получим extend.

Надо заметить, что он хранит 2 стека, поэтому операции выполняются за константу.

```haskell
data [a] = [] | a : [a]
data ListZipper a = LZ [a] a [a]  -- allows to focus on a single element

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft  (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _ = error "listLeft"

listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs
```

Инстанс комонады:
```haskell
extract :: ListZipper a -> a
extract (LZ _ x _) = x

duplicate :: ListZipper a -> ListZipper (ListZipper a)
duplicate = mkZipper listLeft listRight

iterate :: (a -> a) -> a -> [a] -- [x, f x, f (f x), f (f (f x)), ..]
tail :: [a] -> [a] -- элементы кроме первого

-- [f x, f (f x), ..]
iterateTail :: (a -> a) -> a -> [a] 
iterateTail f = tail . iterate f

mkZipper :: (v -> v) -> (v -> v) -> v -> ListZipper v
mkZipper genLeft genRight e = 
  LZ (iterateTail genLeft e) e (iterateTail genRight e)
```

# Grid
```haskell
-- верхний LZ фокусируется на 1 строку из многих, 
-- внутренний на 1 элемент строки
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }  -- 2D grid

-- up/down просто перефокусируются на массив фокусов выше/ниже
-- left/right перефокусируют каждый из фокусов строки на 1 влево/вправо
left, right, up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

-- прочитать элемент под фокусом
gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

-- записать под фокус
gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine
	
-- Получаем фокусы на все строки/столбцы сетки
horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = mkZipper left right
vertical   = mkZipper up   down

instance Comonad Grid where
    extract :: Grid a -> a
    extract = gridRead

	-- получить все столбцы от применения vertical, 
	-- получить из них все строки от применения horizontal,
	-- заернуть конструктором Grid
    duplicate :: Grid a -> Grid (Grid a)
    duplicate = Grid . fmap horizontal . vertical
```

### Нахуя, а главное зачем
Чтобы сделать жизнь, например
```haskell
aliveCount :: [Bool] -> Int
aliveCount = length . filter id

-- массив геттеров для каждого соседа
neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [left, right]
        verticals   = [up, down]
		
-- Посчитать соседей, для каждого геттера применим его и
-- возьмем состояние соседа
aliveNeighbours :: Grid Bool -> Int
aliveNeighbours g = aliveCount 
                  $ map (\direction -> extract $ direction g) neighbours
				  
-- само правило, будет применено для каждой клетки
rule :: Grid Bool -> Bool
rule g = case aliveNeighbours g of
     2 -> extract g
     3 -> True
     _ -> False

-- применяем правило к сетке
evolve :: Grid Bool -> Grid Bool
evolve = extend rule
```

# Traced
### Env
Хотим тащить через функции `(a, b)`, но при этом менять только `b`. Тогда можем сделать комонаду
```haskell
extract (_, b)  = b
extend f (a, b) = (a, f b)
```
и радоваться. 
```haskell
extract :: Pos1D -> Int
extract pos = snd pos

extend :: ((e, a) -> b) -> (e, a) -> (e, b)
extend f w = (fst w, f w)

-- left, right :: (e, a) -> b
left, right :: (Int, Int) -> Int

ghci> extract (start 0 =>> right 3 =>> left 7)
-4
ghci> extract (start 0 =>> right 3 =>> left 7 =>> fst =>> right 5)
5
```
### Builder
Хотим накопить `[Option]`, а после из них создать `Config`. Делаем комонаду
```haskell
newtype ConfigBuilder a = CB ([Option] -> a)

extract (CB f)  = f []
extend :: (ConfigBuilder a -> b) -> ConfigBuilder a -> ConfigBuilder b
--extend 
--  :: (([Option] -> a) -> b)
--  -> ([Option] -> a)
--  -> ([Option] -> b)
extend f (CB oldBuilder) = 
  \newOptions -> 
    f $ CB $ oldBuilder (\options -> [newOptions ++ options])
```

#todo осознать как работает билдер

### Минутка обощения
```haskell
newtype Traced m a = Traced { runTraced :: m -> a }

instance Monoid m => Comonad (Traced m) where
    extract :: Traced m a -> a
    extract (Traced getter) = getter mempty

    extend :: (Traced m a -> b) -> Traced m a -> Traced m b
    extend f (Traced getter) = 
      Traced $ \shift -> f (Traced $ \index -> getter (shift <> index))
```

Просто получает какой-то моноид, когда делаем `extend` мы передаем ему функцию, которая раскроет `Traced` и получит из него какое-то значение. 

С одной стороны мы можем применять любые функции к элементу добавляя их через `=>>`, с другой мы получаем универсальную штуку которой можно скормить любой элемент через геттер (но все это эквивалентно плюсам от простой композиции функций(?))
Но главный плюс, что оно инкапсулирует предыдущее значение(?)
Но ведь каждый обработчик знает его...

```haskell
module Main where
import Control.Comonad.Traced

-- немного инстансов для интов
instance Semigroup Int where
  a <> b = a + b

instance Monoid Int where
  mempty = 0
  mappend a b = a <> b

-- немного сдвигов, выход за границы массива нормален, 
-- если мы его исправим
f, f1, f2 :: Traced Int Int -> Int
f traced = runTraced traced 5
f1 traced = runTraced traced (-13)
f2 traced = runTraced traced 10 

ith :: [Int] -> Int -> Int
ith l idx = l !! idx

-- выведет 5 - 13 + 10 = 2 элемент массива
main = do 
  print $ extract $ traced (ith [1, 2, 3, 4, 5]) =>> f =>> f1 =>> f2

-- Output: 3
```

# зачем
- инкапсуляция, когда не дает фокус на часть объекта и тащит ее сквозь вычисления незаметно для нас
- фокус + можно получать информацию об окружении, если зиппер, например, и мапить основываясь и на ней тоже