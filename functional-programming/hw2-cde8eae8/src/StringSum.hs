module StringSum 
  ( stringSum
  ) where 

import Text.Read 

stringSum :: String -> Maybe Int
stringSum str = fmap sum $ traverse readMaybe (words str)

