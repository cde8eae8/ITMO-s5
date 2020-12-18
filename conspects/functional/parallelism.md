#colloc2
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

# forkIO плохо
Не стоит его использовать.

С ним приходится управлять тред id вручную, например если его потерять, мы никогда не завершим дочерний тред. И часто разумно завершать дочерние треды если умериает родитель, но этого не происходит при исключениях.

### Функции на замену (пакет Async)
```haskell
-- Control.Concurrent.Async
-- вместо этих скорее всего можно использовать concurrently/run
withAsync    :: IO a -> (Async a -> IO b) -> IO b
wait         :: Async a -> IO a
cancel       :: Async a -> IO ()

-- запускает 2 треда, ждет оба
concurrently :: IO a -> IO b -> IO (a, b) 
-- запускает 2 треда, ждет первый, убивает второй
race         :: IO a -> IO b -> IO (Either a b)

-- параллельный мап
mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b) 

test4 :: String -> String -> (ByteString, ByteString)
test4 url1 url2 = 
    withAsync (getURL url1) $ \a1 -> do
      withAsync (getURL url2) $ \a2 -> do
          page1 <- wait a1
          page2 <- wait a2
          pure (page1, page2)
```

# Transactions & STM монада
Переложить с одного аккаунта на другой в многопотоке используя IORef разделенный между потоками.

IORef не синхронизированы на чтение из разных потоков(!)

Один из способов решения - неблокирующие операции.

У нас есть `TVar`, когда мы из разных потоков пишем в нее, мы пишем CASом. Есть монада `STM`, которая всем этим и занимается.

В STM только чистый код, потому что мы повторяем одно действие несколько раз. Есть исключения. Есть `retry`. 

```haskell
-- import Control.Concurrent.STM
data STM a  -- software transactional memory
instance Monad STM where

atomically :: STM a -> IO a

data TVar a -- transactional variable
newTVar   :: a -> STM (TVar a)
readTVar  :: TVar a -> STM a
writeTVar :: TVar a -> a -> STM ()

retry     :: STM a                   -- try again current transaction
orElse    :: STM a -> STM a -> STM a -- if first retries then call second

throwSTM  :: Exception e => e -> STM a
catchSTM  :: Exception e => STM a -> (e -> STM a) -> STM a

transfer :: Integer -> Account -> Account -> STM ()
transfer amount from to = do
    fromVal <- readTVar from
    if (fromVal - amount) >= 0
        then do
               debit amount from
               credit amount to
        else retry
```

### retry
Если нам не нравится текущее состояние, то мы можем подождать с помощью `retry` пока не изменится какая-нибудь переменная в блоке из тех что мы читали через `readTVar`

### vs MVar
Медленнее, но сложнее получить дедлок. Пока не используешь `retry`?

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

==**Они вычисляют до WHNF**==
Также из-за этого нет смысла передавать уже вычисленный до WHNF объект

#todo как собирать статистику
#todo зачем rseq + https://stackoverflow.com/questions/34281885/when-are-rpar-and-rseq-expressions-actually-computed-in-haskell-program
#todo 