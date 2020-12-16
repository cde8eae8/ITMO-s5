module WeekdaysTests (tests) where

import Test.Hspec
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import WeekDays
import Control.Monad
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit

instance Eq Weekday where
  (==) day day2 = (show day) == (show day2)

nextDayTest :: Weekday -> Weekday -> TestTree
nextDayTest day result = testCase (printf "%s" (show day)) $ 
                                  assertEqual "" result (nextDay day) 
                  
isWeekendTest :: Weekday -> Bool -> TestTree
isWeekendTest day result = testCase (printf "%s" (show day)) $ 
                                  assertEqual "" result (isWeekend day) 

daysToPartyTest :: Weekday -> Int -> TestTree
daysToPartyTest day result = testCase (printf "%s" (show day)) $ 
                                  assertEqual "" result (daysToParty day) 

afterDaysTest :: Int -> Weekday -> Weekday -> TestTree
afterDaysTest sh day result = testCase (printf "%d %s" sh (show day)) $ 
                                  assertEqual "" result (afterDays sh day) 

nextDayTests = testGroup "nextDay" $
                  [ nextDayTest Monday    Tuesday
                  , nextDayTest Tuesday   Wednesday
                  , nextDayTest Wednesday Thursday
                  , nextDayTest Thursday  Friday
                  , nextDayTest Friday    Saturday
                  , nextDayTest Saturday  Sunday
                  , nextDayTest Sunday    Monday
                  ]

isWeekendTests = testGroup "isWeekend" $
                  [ isWeekendTest Monday    False 
                  , isWeekendTest Tuesday   False 
                  , isWeekendTest Wednesday False 
                  , isWeekendTest Thursday  False 
                  , isWeekendTest Friday    False 
                  , isWeekendTest Saturday  True
                  , isWeekendTest Sunday    True 
                  ]

daysToPartyTests = testGroup "daysToParty" $
                  [ daysToPartyTest Monday    4 
                  , daysToPartyTest Tuesday   3 
                  , daysToPartyTest Wednesday 2 
                  , daysToPartyTest Thursday  1 
                  , daysToPartyTest Friday    0 
                  , daysToPartyTest Saturday  6 
                  , daysToPartyTest Sunday    5 
                  ]

afterDaysTests =  testGroup "afterDays" 
                  [ afterDaysTest 1  Monday    Tuesday 
                  , afterDaysTest 2  Tuesday   Thursday  
                  , afterDaysTest 3  Wednesday Saturday  
                  , afterDaysTest 4  Thursday  Monday    
                  , afterDaysTest 5  Friday    Wednesday 
                  , afterDaysTest 6  Saturday  Friday    
                  , afterDaysTest 7  Sunday    Sunday    
                  , afterDaysTest 14 Sunday    Sunday    
                  , afterDaysTest (-1) Sunday    Saturday 
                  , afterDaysTest (-2) Sunday    Friday
                  , afterDaysTest (-3) Sunday    Thursday
                  , afterDaysTest (-4) Sunday    Wednesday
                  ]
 
instance Arbitrary Weekday where
  arbitrary = elements [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]


afterDaysProperties = testGroup "afterDays" 
                          [ QC.testProperty "period" $ 
                              (\x day -> afterDays (x :: Int) (day :: Weekday) == afterDays (x + 7) day)
                          , QC.testProperty "identity" $ 
                              (\day -> afterDays 0 (day :: Weekday) == day)
                          ]

tests :: TestTree
tests = testGroup "Weekdays" 
  [
    testGroup "Unit tests" [nextDayTests, isWeekendTests, daysToPartyTests, afterDaysTests],
    testGroup "Property tests" [afterDaysProperties]
  ]


