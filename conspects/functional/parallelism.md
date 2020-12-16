# Общая идея тредов
Есть треды ОС, есть треды хаскеля по несколько на тред ос, и есть спарки - еще более легкие треды в хаскель тредах

# IO
```haskell
forkIO :: IO () -> IO ThreadId  -- creates lightweight thread

import Control.Concurrent

main = do
  _threadId <- forkIO $ do
    threadDelay 1000000
    putStrLn "Forked thread awake"
  threadDelay 2000000
  putStrLn "Main thread finishes"
```

```bash
$ ghc -threaded -o test Test.hs
$ ./test +RTS -N2

Forked thread awake
Main thread finishes
```

Надо использовать флажки `+RTS -N2` чтобы была многопоточность. Либо установить эти флажки для stack чтобы он сам собирал.
Без них программа будет однопоточной с Round-robin алогоритмом переключения между тредами.

# Mutex
```haskell
data MVar a  -- empty or full box

newEmptyMVar :: IO (MVar a)           -- create empty box
putMVar      :: MVar a -> a -> IO ()  -- fill box with value
takeMVar     :: MVar a -> IO a        -- take var with block
```
Можно положить значение и забрать значение. Если она пустая, кладем. Если нет - ждем пока ее не освободят, потом кладем. 
Может быть дедлок если никто на может забрать значение. При этом хаскель бросает исключение, если ссылки на MVar есть только из спящих на нем потоков.

# Kill...
Можно посылать треду асинхронные исключения (например через Ctrl+C или функции `throwTo`) из других тредов. 
```haskell
throwTo :: Exception e => ThreadId -> e -> IO ()

killThread :: ThreadId -> IO ()
killThread tid = throwTo tid ThreadKilled
```

Пример обработчика Ctrl+C
```haskell
main =
  handle intrHandler $
  flip mapM_ [1..1000] $ \i -> do
    threadDelay 1000000
    putStrLn $ "Finished round " <> show i

intrHandler :: AsyncException -> IO ()
intrHandler UserInterrupt = putStrLn "Finishing due to user interrupt ..."
intrHandler e = putStrLn $ "Caught async exception: " <> show e
```

# Classification
```haskell
-- Synchronous
throwIO :: Exception e => e -> IO a
throw :: Exception e => e -> a -- чтобы кидать в чистом коде

-- Asynchronous
throwTo :: Exception e => ThreadId -> e -> IO ()
```
В рантайме нет разницы между синхронными и асинхронными исключениями.

# Catch me if you can
```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a

handle :: Exception e => (e -> IO a) -> IO a -> IO a
handle = flip catch
```
Может поймать все типы исключений. Единственный способ ловить исключения.
Чистые исключения можно ловить с помощью catch. Чистые исключения в чистом коде не поймать, надо докинуть до IO (например main).

==**NB**: ExceptT никак не относится к этому - это просто эмуляция исключений с помощью Either.==

# Masks
Во время обработки асинхронного исключения может броситься еще одно. Мы этого не хотим.
Есть функция `mask_ :: IO a -> IO a`, она выполнит переданную функцию, а если в этот момент кто-то сделает `throwTo`, он будет заблокирован до выхода из функции.
```haskell
main = action `catch` \e -> do
         printError e
         (mask_ cleanup)
```

Надо быть внимательным с тем что пока программа в `mask_` ее не прервать через Ctrl+C, только через `SIGKILL`.
Поэтому ставить долгие действия в `mask_` плохо. Сюда же относятся и отправка сообщений по сети в `mask_` и подобные долгие действия.

# bracket
```haskell
bracket :: IO a -> (a -> IO b) -> (b -> IO c) -> IO c
finally :: IO a -> IO b -> IO a
```

# Заметки
`try` и `finally` это функции-хелперы. Только функция `catch` является основополагающей. 

Чтобы отличать синхронные и асинхронные исключения, можно просто бросать разные типы исключений.

Можно испоьзовать библиотеку `safe-exceptions` чтобы понимать асинхронное ли исключение.

# forkIO
Не стоит его использовать.

Надо управлять тред id вручную, например если его потерять, мы никогда не завершим дочерний тред. И часто разумно завершать дочерние треды если умериает родитель, но этого не происходит при исключениях.

### Функции на замену (пакет Async)
```haskell
-- Control.Concurrent.Async
-- вместо этих скорее всего можно использовать concurrently/run
withAsync    :: IO a -> (Async a -> IO b) -> IO b
wait         :: Async a -> IO a
cancel       :: Async a -> IO ()

-- запускает 2 треда, ждет оба
concurrently :: IO a -> IO b -> IO (a, b) 
-- запускает 2 треда, ждет первый, уббивает второй
race         :: IO a -> IO b -> IO (Either a b)

-- параллельный мап
mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b) 
```

# Transactions & STM монада
Переложить с одного аккаунта на другой в многопотоке используя IORef разделенный между потоками.
#todo что да как, еще много

---

# Параллелизм & sparks
Мы чистые и объекты может менять только наш поток, если это чистое вычисление. 
- Нет записей (все немутабельно)
- Нет гонок
- Не нужны локи
- Нет побочных эффектов
- Можно считать в любом порядке

```haskell
data Eval a                  -- Eval is monad for parallel computation
instance Monad Eval where

runEval :: Eval a -> a  -- pull the result out of the monad

rpar :: a -> Eval a  -- suggest to parallel, create *spark* 
rseq :: a -> Eval a  -- wait for evaluation of argument (eval it to WHNF)
```

С запуском `runEval` мы подключаемся к тредпулу спарков и отдаем ему задачи на исполнение.

```haskell
runEval $ do
    a <- rpar (f x) -- не ждем пока посчитается
    b <- rpar (f y)
    return (a, b)
```

#todo как собирать статистику
#todo зачем rseq + https://stackoverflow.com/questions/34281885/when-are-rpar-and-rseq-expressions-actually-computed-in-haskell-program
#todo 