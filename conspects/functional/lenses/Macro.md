# -XCPP
Подрубает препроцессор плюсов
Те же минусы что и у плюсового - небезопасный

# Template Haskell
DSL для хаскеля в хаскеле?...

`makeLenses` для [[Lens]] использует его чтобы нагенерить реализации линз

```haskell
-- AST of lambda \(x,_,_) -> x
LamE [TupP [VarP x_1,WildP,WildP]] (VarE x_1) 
```

```haskell
import Language.Haskell.TH

fst3 :: Q Exp
fst3 = do
    x <- newName "x"
    pure $ LamE [TupP [VarP x, WildP, WildP]] (VarE x)
```

`$(fst3)` - сплайс, разворачивает темплейтную хрень в реальный код
```haskell
ghci> :t fst3
fst3 :: Q Exp  -- Q is some monad

ghci> :t $(fst3)
$(fst3) :: (t2, t1, t) -> t2
```

Для тапла любой длины:
```haskell
fstN :: Int   -- ^ take tuple «length» as argument
     -> Q Exp
fstN n = do
    x <- newName "x"
    lamE [tupP $ varP x : replicate (n - 1) wildP] (varE x)
```

==**НО**==: Использовать макрос нельзя в том модуле, где он объявлен

# -XQuasiQuotes

Парсит на этапе компиляции хаскельный код в синтаксическое дерево. 

```haskell
ghci> :set -XTemplateHaskell
ghci> :set -XQuasiQuotes
ghci> runQ [| \x -> x + 3 |]
LamE [VarP x_2] (InfixE (Just (VarE x_2)) (VarE GHC.Num.+) (Just (LitE (IntegerL 3))))
```

Есть другие применения этой штуки **И ТЕБЕ НАДО ПРО НИХ ПРОЧИТАТЬ**
И можно писать свои.

### Немного бесплатных инстансов
```haskell
{-# LANGUAGE TemplateHaskell #-}

module CustomShow where

import Language.Haskell.TH

emptyShow :: Name -> Q [Dec]
emptyShow name = [d|instance Show $(conT name) where show _ = ""|]
```
```haskell
{-# LANGUAGE TemplateHaskell #-}

import CustomShow (emptyShow)

data MyData = MyData
     { foo :: String
     , bar :: Int
     }

emptyShow ''MyData  -- ''MyData == mkName "MyData"
-- ^ Expands to: instance Show MyData where show _ = ""

main = print $ MyData { foo = "bar", bar = 5 }
```

### Читаем файлики...
Можно прочитать файлик во время компиляции, пропарсить в выражения хаскеля и потом тайпчекнуть. Если не тайпчекнулось, упадет во время компиляции.