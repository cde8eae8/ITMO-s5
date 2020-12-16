```haskell
main = do
	fileContent <- readFile "foo"
	writeFile "foo" ('a':fileContent)
	readFile "foo" >>= putStrLn
```

Внутри readFile есть хак на ленивое чтение файла, поэтому он не будет закрыт после выполнения readFile. Это не нарушает инвариант что монада вычисляет выражения по порядку.

```haskell
hGetContents :: Handle -> IO String
```

---

Для IO лучше юзать либы.
