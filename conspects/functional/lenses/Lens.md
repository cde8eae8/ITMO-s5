# Наивный мир
Хотим обращаться удобно со влолженными типами
"Фокусируемся на глубоко вложенное поле объекта"
Сделаем линзы:

```haskell
data Lens obj field = Lens
     { view :: obj -> field -- геттер
     , set  :: field -> obj -> obj -- сеттер	
     }
	 
	 
-- Линза = геттер + сеттер
view :: Lens obj field -> obj -> field
set  :: Lens obj field -> field -> obj -> obj

-- Поднять модификацию поля до модификации объекта
over :: Lens obj field -> (field -> field) -> (obj -> obj)
over lens updater obj = set lens (updater $ view lens obj) obj
```

Пример:
```haskell
personAddressLens :: Lens Person Address
personAddressLens = 
  Lens address   										 -- getter
       (\fn obj -> obj { address = fn (address obj) })   -- setter

addressCityLens :: Lens Address City
addressCityLens = 
  Lens city 									   -- getter
  	   (\fn obj -> obj { city = fn (city obj) })   -- setter
```

### Lenses compositing
```haskell
personAddressLens :: Lens Person Address
personAddressLens = Lens address (\fn obj -> obj { address = fn (address obj) })

addressCityLens :: Lens Address City
addressCityLens = Lens city (\fn obj -> obj { city = fn (city obj) })

personCityLens :: Lens Person City
personCityLens = 
    Lens (view addressCityLens    . view personAddressLens)
         (over personAddressLens  . over addressCityLens)

-- общий случай
.> :: Lens a b -> Lens b c -> Lens a c
top .> down = Lens (view top . view down)
						(over down . over top)
```

# Жестокая реальность
```haskell
type Lens  s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
type Lens' s   a   = Lens s s a a
type Lens' s   a   = forall f. Functor f => (a -> f a) -> (s -> f s)
```
Заметим, что это просто **алиас** на тип функции.
Получаем функцию которая апдейтит поле и пихает в функтор, а из не получаем функцию которая апдейтит объект и запихивает его в этот же функтор. 
```haskell
over :: Lens' s a -> (a -> a) -> (s -> s)
over lens f s = runIdentity $ lens (\v -> Identity . f) s

newtype Const a x = Const { getConst :: a }
instance Functor (Const a) where
    fmap _ (Const v) = Const v

-- Const тащит в себе первый аргумент и игнорит все преобразования
-- Таким образом мы просто протащим a через все fmap
view :: Lens' s a -> s -> a
view lens s = getConst $ l Const s 

set  :: Lens' s a -> a -> (s -> s)
set lens v = over lens (const v)
```

А еще кроме урезанных примеров выше можно например передать функцию `a -> [a]` или `a -> Maybe a` и получить такое же для `s`.

### Правила
```haskell
-- запись меняет то что мы видим на то что мы записали
view l (set l field obj)      ≡ field

-- запись того что и так есть = id
set l (view l obj) obj   	  ≡ obj

-- запись перезаписывает прошлое значение полностью
set l field (set l field obj) ≡ set l field obj
```

### Непростые линзы `Lens s t a b`
Если мы например имеем параметризованное типом поле, мы можем поменять тип этого поля и тип поля `s`

### Операторы
- Начинается с ^ - в духе view, .^ - view
- ends with `~` - over/set; `.~` - set, `%~` - over
- содержат `.` - базовые
- содержат `%` - принимают функции
- содержат `=` - эквивалентны штуке с `%`, но в `State` монаде

### Композиции
Теперь линза это просто функция. Значит их композиция работает через `.`
```haskell
ghci> initialState^.boss.position.x
	   ^-- value   |  ^-----------^ <----lenses composition
	   			   ^-- operator view
```
# Traverse
Фокусировка на множество объектов (где множество может быть пустым, в отличие от линз).
Короче линза с аппликативом.
```haskell
type Traversal s t a b 
    = forall f . Applicative f => (a -> f b) -> (s -> f t)
type Traversal' obj field 
    = forall f . Applicative f => (field -> f field) -> (obj -> f obj)
```

### & Traversable
```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse :: (Applicative f, Traversable t) => (a -> f b) -> (t a -> f (t b))
traverse :: Traversal (t a) (t b) a b 

traversed :: Traversable t => Traversal' (t a) a
traversed = traverse
```

#todo Подумать как они связаны...

### Композиция
Можно композировать с линзами
```haskell
(.) :: Lens'      a b -> Traversal' b c -> Traversal' a c
(.) :: Traversal' a b -> Lens'      b c -> Traversal' a c
```
И между собой

### Getting
#todo getting

# Prisms
#todo что такое Choice и Applicative и зачем
```haskell
type Prism s t a b 
    = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
```

Позволяют фокусироваться на конкретном конструкторе типа. Если линзы для произведений типов, то это для сумм.
Композируется с линзами и траверсами, поэтому можно фокусироваться на конкретный конструктор и дальше вниз.

```haskell
-- Если мы в том конструкторе, то получаем значение
preview :: Prism' s a -> s -> Maybe a

-- Обернуть в тот конструктор
review :: Prism' s a -> a -> s
```