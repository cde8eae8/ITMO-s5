module SplitTests (tests) where

import Test.Hspec
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Split
import Control.Monad
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty as N

splitTest :: (Show a, Eq a) => a -> [a] -> [[a]] -> TestTree
splitTest ch list result = testCase (show list) $ 
                                  assertEqual "" (N.fromList result) (split ch list)

joinTest :: (Show a, Eq a) => a -> [[a]] -> [a] -> TestTree
joinTest ch list result = testCase (show list) $ 
                                  assertEqual "" result (Split.join ch (fromList list))

splitTests = testGroup "split" $
                  [ splitTest '/' "a/b/c/d" ["a", "b", "c", "d"] 
                  , splitTest '/' "a1/b2/c3/d4" ["a1", "b2", "c3", "d4"] 
                  , splitTest '/' "/b2/c3/d4" ["", "b2", "c3", "d4"] 
                  , splitTest '/' "a1/b2/c3/" ["a1", "b2", "c3", ""] 
                  , splitTest '/' "/b2/c3/" ["", "b2", "c3", ""] 
                  , splitTest '/' "" [""] 
                  , splitTest '/' "/" ["", ""] 
                  , splitTest '/' "///" ["", "", "", ""] 
                  ]
                  
joinTests = testGroup "join" $
                  [ joinTest '/' ["a", "b", "c", "d"] "a/b/c/d"
                  , joinTest '/' ["a1", "b2", "c3", "d4"] "a1/b2/c3/d4" 
                  , joinTest '/' ["", "b2", "c3", "d4"] "/b2/c3/d4" 
                  , joinTest '/' ["a1", "b2", "c3", ""] "a1/b2/c3/" 
                  , joinTest '/' ["", "b2", "c3", ""] "/b2/c3/" 
                  , joinTest '/' [""] ""
                  , joinTest '/' ["", ""] "/" 
                  , joinTest '/' ["", "", "", ""] "///" 
                  , joinTest '/' ["a"] "a"
                  ]

tests :: TestTree
tests = testGroup "Split" 
  [ testGroup "Unit tests" [splitTests, joinTests]
  , testGroup "Property tests" 
    [ SC.testProperty "join . split == id" $ 
        (\list ch -> Split.join ch (split (ch :: Char) (list :: [Char])) == list )
    ]
  ]

