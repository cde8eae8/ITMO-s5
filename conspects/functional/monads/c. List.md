> Semigroup [a]
Monoid [a]
Foldable
Functor
Applicative
Alternative
Traversable
Monad

Моноид по конкатенации
Foldable ~ reduce
Функтор это map
Апликатив это применить n функций к k значениям `[f x | f <- fs, x <- xs]`
Альтернатив это конкатенация списков
Монада это применить к каждому элементу списка в монаде операцию и сконкатенировать результаты