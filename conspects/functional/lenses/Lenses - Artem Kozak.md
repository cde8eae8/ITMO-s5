from [part1](https://artyom.me/lens-over-tea-1)

---

# Linses

Мы хотим линзы...

### Getter + Setter
```haskell
data Lens s a = Lens
  { getter :: s -> a  		-- по структуре поле
  , setter :: a -> s -> s } -- по новому значению и структуре обновить поле
```

Здесь есть проблема - чтобы обновить значение нам нужен геттер + сеттер, например линза для обновления элемента листа пройдет по нему дважды...
А еще сеттер мог бы переиспользовать код сеттера

### Remove getter
```haskell
-- Изменить значение + вернуть старое значение и новую структуру
-- Геттер это Const как первый агумент
-- Сеттер это проигнорить возвращаемое значение
type Lens s a = (a -> a) -> s -> (a, s)
```

### Functor

```haskell
type Lens s a = Functor f => (a -> f a) -> s -> (a, f s)

-- example:
-- Дойдем до нужного элемента и применим к нему переденную функцию, 
-- получим функтор. Поместим в этот функтор хвост, а затем будем
-- расширять добавляя элементы в голову списка в функторе.
-- То есть мы 
-- 0. Получаем поле типа а объекта типа s
-- 1. Вернем функтор параметризованный типом а
-- 2. Засунем в этот функтор остатоки объекта s
-- 3. Вернем старое значение поля + новый объект в функторе
ix :: Int -> Lens [a] a
ix index f list
  | index < 0        = error "ix: negative index"
  | null list        = error "ix: index too large"
  | old:rest <- list = if index == 0
                         then (old, (: rest) <$> f old)
                         else second ((old :) <$>) $ ix (index-1) f rest
```

### Инкапсулируем возвращаемое значение
```haskell
data Storey x f a = Storey x (f a)
  deriving Show

instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)
  
type Lens s a = Functor f => (a -> f a) -> s -> f s
  
>>> ix 2 (\x -> Storey x [1..x]) [300, 100, 4, 600, 900, 400]
```

Теперь реализация линзы не парится о возвращаемом значении, мы просто в update function возвращаем `(Store x functorValue)` и оно протаскивается дальше.

### Real world
```haskell
-- более общая версия - можно менять типы
type Lens s t a b = Functor f => (a -> f b) -> s -> f t

-- то что было раньше
type Lens' s a = Functor f => (a -> f a) -> s -> f s
```

#TODO Compose
#TODO Const
#TODO Identity
