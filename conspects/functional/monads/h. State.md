Судя по всему аналогично Reader и IO это просто формирует цепочку вычислений, а потом она считается

```haskell
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return :: a -> State s a
    return a = State $ \s -> (a, s)

    (>>=) :: State s a -> (a -> State s b) -> State s b
    State run >>= f = State (\s -> 
		let (v, new_s) = run s in
			(runState (f v)) new_s)
```

#todo

```haskell
get       :: State s s
put       :: s -> State s ()
modify    :: (s -> s) -> State s ()
gets      :: (s -> a) -> State s a
withState :: (s -> s) -> State s a -> State s a
evalState :: State s a -> s -> a
execState :: State s a -> s -> s
```