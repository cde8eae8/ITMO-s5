`getChar :: Char` <-- ожидается что это констатнта, так как язык чистый + ленивые вычисления -- хз когда мы считаем ввод
`getChar :: Int -> Char` <-- могут быть разные результаты, но надо контролировать этот int чтобы не сколлизилось
`getChar :: Int -> (Char, Int) <-- уже лучше, пока не запутаемся в инте, который возвращаем

```haskell
get2Chars :: Int -> (Char, Int)
get2Chars i = ([a,b], i2) where (a, i) = getChar i1, (b, i2) = getChar i1
```
Хотим запихнуть хендл в монаду

`type IO a = RealWorld -> (a, RealWorld)` <-- RealWorld не существует, выражает "внешнее окружение"
`getChar :: IO Char (:: RealWorld -> (Char, RealWorld))``

```haskell
main :: IO () (:: RealWorld -> ((), RealWorld))
main world0 = 
let (a, world1) = getChar world0, 
	(b, world2) = getChar world1 
	in ((), world2) <-- если этого не будет, ленивые вычисления выкинут все getChar
```

```haskell
newtype IO a = { unIO :: State# RealWorld  -> (State# RealWorld, a) }
instance Monad IO where 
	IO m >>= k = IO $ \s ->
		case m s of 
			(new_s, a) -> unIO (k, a) new_s
```
Просто хранит внутри себя цепочку вычислений. Байнд это вычислить цепочку внутри и накинуть сверху вызов новой функции. `main :: IO()`, в него передастся RealWorld и вычислится вся цепочка монады внутри main"