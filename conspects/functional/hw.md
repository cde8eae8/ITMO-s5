# HW 4
- строгие поля
- 
# HW 3
Операции: 
- cd
- ls `listDirectory`
- create file/dir `createFile` `createDir`
- remove file/dir `removeFile` `removeDir`
- cat `read`
- write `write`
- find = ls + get type
- fileinfo:
	- path ok
	- rwx `getPermission`
	- extension `getExtensionFromPath` - System.FilePath
	- creation/modification time `getModificationTime`/`getAccessTime`
	- size  `getFileSize`
- dir info:
	- path ok
	- rwx `getPermission`
	- size `getFileSize` + recursive
	- n files ls
FS:
```haskell
  read/write
  createFile createDir
  removeFile removeDir
  listDirectory
  getPermission
  getExtensionFromPath
  getModificationTime/getAccessTime
  getFileSize
```

- Обработка ошибок
- гуи
- посмотреть за что минусы в табличке и исправить это
- проверить линтером
- явные импорты
- явные экспорты
- проверить либы в кабале
- StateT -> Reader IORef
- убрать ненужное из exposed modules
- что везде явные экспорты/импорты
- rename FS -> FileSystem
- убрать ненужные файлы
- подумать какое состояние надо возвращать при исключении и какое возвращается (`wrapIOExceotion`) (СИСТЕМА ДОЛЖНА БЫТЬ АТОМАРНОЙ!!!)
- научиться инициализировать root
- переписать хендлинг исключений
- проверить Ctrl C и Ctrl D
- Возможно отделить тайпкласс getEnv от самого Env? Выделить функции получения полей, записи полей в тайпкласс, чтобы в моке не зависеть от Env
- подумать что такое FlexibleInstances
- переписать focusPath на что-то вроде fmap/applicative/monad
- убрать toTree/fromTree в тестах
- убрать ExceptT так как мы все равно можем бросать из чистого кода (!!!!!)
- экспортировать из мокфс только конструктор "/" чтобы другое нельзя было создать
- переписать runMockFS
- переименовать ModifyEnv, потому что он позволяет не только модифицировать
- разделить мокфс и монаду для тестов
- запретить в мок системе создание двух папок с одинаковыми именами
- может быть ловить исключения через `try`?
---

# Codestyle
- [x] if then else
- [x] type signatures для всех на топлевел
- [x] все функции в where с типами
- [x] длинные типы функций - на разных строках каждый
- [x] You must not declare records with multiple constructors because their getters are partial functions.
- [x] импорты: все извне, все из проекта, все из текущей цели
- [x] экспорты: сортить по алфавиту, но все типы вначале, потом функции; все на новой строке; не использовать plural
- [x] You must provide a type signature for every top-level definition.
- [ ] **You must comment every exported function and data type.**
- [ ] **You should comment every top-level function.**
- [ ] **You must use Haddock syntax in the comments.**
- [x] Blank Lines: 
	- 1 между топ объявлениями
	- 0 между типом и функцией
	- 1 между функциями инстанса тайпкласса
	- 1 между объявлениями where/let если большие объвления
# HW2
Осталась документация, тесты на Applicatible & TraversableTree и дописать у него Traversable и линтер
- [ ] documentation...
- [x] code style...

### Functor
- [x] Sum numbers in string
	- [x] Task
	- [x] Tests
	- [ ] Property tests
- [x] Tree
	- [x] Task _Rules_ _Traversable_
	- [x] Tests
- [x] NonEmpty
	- [x] Task _Rules_
	- [x] Tests

### Monads
- [x] Expressions
	- [x] Task
	- [x] Tests
- [x] Moving average _Rules_
	- [x] Task
	- [x] Tests

### Parsers
- [x] Copy-paste _Rules_
	- [x] Task
	- [x] Tests
	- [ ] Property tests
	- http://slides.com/fp-ctd/lecture-55#/10/0/3
	- Functor tests
- [x] Base combinators
	- [x] Task
	- [x] Tests
	- [ ] Property tests
- [x] Simple parsers
	- [x] Task
	- [x] Tests
	- [ ] Property tests
- [x] Nonsimple parsers
	- [x] Task
	- [x] Tests
	- [ ] Property tests

### Bonus
- [ ] Bonus