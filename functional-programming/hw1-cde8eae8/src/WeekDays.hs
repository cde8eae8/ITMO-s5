module WeekDays (Weekday(..), nextDay, afterDays, isWeekend, daysToParty) where

  
data Weekday = Monday 
             | Tuesday 
             | Wednesday 
             | Thursday 
             | Friday 
             | Saturday 
             | Sunday 
              deriving Show

nextDay :: Weekday -> Weekday
nextDay Monday    = Tuesday    
nextDay Tuesday   = Wednesday  
nextDay Wednesday = Thursday  
nextDay Thursday  = Friday    
nextDay Friday    = Saturday  
nextDay Saturday  = Sunday
nextDay Sunday    = Monday    

afterDays :: Int -> Weekday -> Weekday
afterDays n day 
  | n < 0       = afterDays (7 + n `mod` 7) day
  | n > 6       = afterDays (n `mod` 7) day
  | n == 0      = day
  | otherwise   = afterDays (n - 1) (nextDay day)

isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Weekday -> Int
daysToParty Friday = 0
daysToParty day    = (daysToParty $ afterDays 1 day) + 1
