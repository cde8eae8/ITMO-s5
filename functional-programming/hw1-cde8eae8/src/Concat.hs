module Concat (maybeConcat) where

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat list = foldl 
