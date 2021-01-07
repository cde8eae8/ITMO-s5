# HW 4
- строгие поля


# GUI
- не терять стейт в гуи
- проверить что find ищет только файлы
- [x] В ГУИ СРАЗУ ПОКАЗЫВАТЬ СПИСОК ФАЙЛОВ!!!!!
- [x] нужна обработка ошибок
- [x] канонализация путей
- [x] **уметь принимать другой рут как аргумент!!!**
- [x] поменять парсер аргументов в консольном варрианте
- [x] **УБРАТЬ ERRORы**
- ловить не все исключения, а только свои
- [x] ls не в текущей не работает и info
- [x] **ДОБАВИТЬ HELP**
- сделать инструкцию как пользоваться
- переписать хендлеры которые юзают Maybe
- [x] надо дописать запрет на выход за пределы рута
- ~~Убрать 13 для KeyCode энтера~~
- ~~заменить accumB на степпер~~
- ~~не падать если открываем бинарные файлы~~
- написать что стрелки не работают
- [x] cd
- [x] ls
- [x] create file/dir
- [x] remove file/dir
- [x] cat
- [x] write
- [x] find
- [x] fileinfo
- [x] dir info
Виджеты:
- Текущая папка с сортировкой на папка/файл
- Кнопка "наверх"
- ~~Даблтап~~ "Enter" по папке - переход в нее
- ~~Даблтап~~ "Enter" по файлу - открыть на редактирование
- Кнопка "сохранить"
- Кнопка поиск
- Создать папку/файл
- Удалить папку/файл
- Тап по файлу/папке - вывести информацию
Behavior:
Общее:
- при cd
	- очистить fileinfo
	- не мувать выше корня
	- не мувать если открыт файл
Реализация:
- bCurrentDirectory
	- при нажатии "вверх" записать в себя другую папку
		- Если bFileViewer с файлом, бросить ошибку что открыт файл
	- при нажатии "Enter" посмотреть на bSelection и если под ним папка, перейти в нее
- bSelection
	- Нажатие - перевыделить, хз
	- 
- bFileViewer
	- при нажатии Enter посмотреть на bSelection и если под ним файл, открыть его
- bInfo
	- При клике по кнопке из списка(???) вывести информацию о штуке
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

- Написать ридми
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

- [x] Убрать стдаут в клиенте гуи
- **Проверять что файл не существует перед созданием**
- [x] понять почему падает при открытии с ошибкой что открываешь папку как файл
- ~~проверить что состояние меняется после всех событий runFS~~
- ~~перевечтии парсинг аргументов в гуи из либы~~
	-  что делать с лс?
- унифицировать поведение мок системы и обычной, особенно проверить mk/rmFile/Dir
- [x] Обработка ошибок
- [x] гуи
- посмотреть за что минусы в табличке и исправить это
- проверить линтером
- явные импорты
- явные экспорты
- проверить либы в кабале
- [x] StateT -> Reader IORef
- убрать ненужное из exposed modules
- что везде явные экспорты/импорты
- rename FS -> FileSystem
- убрать ненужные файлы
- [x] подумать какое состояние надо возвращать при исключении и какое возвращается (`wrapIOExceotion`) (СИСТЕМА ДОЛЖНА БЫТЬ АТОМАРНОЙ!!!)
- научиться инициализировать root
- [x] переписать хендлинг исключений
- [x] проверить Ctrl C и Ctrl D
- [x] Возможно отделить тайпкласс getEnv от самого Env? Выделить функции получения полей, записи полей в тайпкласс, чтобы в моке не зависеть от Env
- [x] подумать что такое FlexibleInstances
- [x] переписать focusPath на что-то вроде fmap/applicative/monad
- [x] убрать toTree/fromTree в тестах
- [x] убрать ExceptT так как мы все равно можем бросать из чистого кода (!!!!!)
- ~~экспортировать из мокфс только конструктор "/" чтобы другое нельзя было создать~~
- ~~переписать runMockFS~~
- ~~переименовать ModifyEnv, потому что он позволяет не только модифицировать~~
- [x] разделить мокфс и монаду для тестов
- [?] запретить в мок системе создание двух папок с одинаковыми именами
- ~~может быть ловить исключения через `try`?~~
- ~~смержить ветки~~
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