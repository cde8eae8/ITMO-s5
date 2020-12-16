```haskell
import System.IO.Unsafe

foo :: ()
foo = unsafePerformIO $ putStrLn "foo"

bar :: String
bar = unsafePerformIO $ do
          putStrLn "bar"
          return "baz"

main = do let f = foo
          putStrLn bar
```
Есть функция `unsafePerformIO`, которая притворяется что твое значение не IO, а просто значение. 

NB: если дважды вызвать foo, "foo" вызовется один раз (он запомнит возвращаемое значение)
NB: в коде выше не выполнится foo, по ленивости

*досмотри*