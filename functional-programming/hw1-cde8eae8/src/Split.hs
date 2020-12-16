module Split (split, join) where

import Data.List.NonEmpty (NonEmpty, fromList, toList)

splitF :: (Eq a) => a -> a -> [[a]] -> [[a]]
splitF char cur list 
  | char == cur = [[]] ++ list
  | otherwise   = [[cur] ++ (head list)] ++ (tail list)

joinF :: a -> [a] -> [a] -> [a]
joinF char cur res = cur ++ [char] ++ res

split :: (Eq a) => a -> [a] -> NonEmpty [a]
split ch list = fromList $ foldr (splitF ch) [[]] list

join :: a -> NonEmpty [a] -> [a]
join ch l = foldr (joinF ch) (last list) (init list)
  where list = toList l
