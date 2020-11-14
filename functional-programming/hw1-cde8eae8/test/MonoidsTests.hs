module MonoidsTests (tests) where

import Test.Hspec
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import Monoids
import Data.Maybe
import Control.Monad
import Text.Printf
import Data.Semigroup
import qualified Data.List.NonEmpty as NN

import Test.Tasty
import Test.Tasty.HUnit

maybeConcatTest :: [Maybe String] -> String -> TestTree
maybeConcatTest testSet result = testCase (show testSet) $ assertEqual "" result (maybeConcat testSet) 

nonEmptyTest :: [NonEmpty Int] -> NonEmpty Int -> TestTree
nonEmptyTest testSet result = testCase (show testSet) $ assertEqual "" result (sconcat $ NN.fromList testSet) 

tests :: TestTree
tests = testGroup "Semigroup & Monoids" $
          [
            testGroup "maybeConcat" $ 
              [ maybeConcatTest [Just "123", Just "456"] "123456"
              , maybeConcatTest [Nothing, Just "123", Nothing, Just "456"] "123456"
              , maybeConcatTest [Just "123", Nothing, Just "456"] "123456"
              , maybeConcatTest [Just "123", Nothing, Just "456", Nothing] "123456"
              , maybeConcatTest [] ""
              , maybeConcatTest [Just "123"] "123"
              , maybeConcatTest [Nothing] ""
              ]
          , testGroup "nonEmptyConcat" $ 
              [ nonEmptyTest [1 :| [2, 3, 4], 2 :| [5, 6, 7]] (1 :| [2, 3, 4, 2, 5, 6, 7])
              , nonEmptyTest [1 :| [2], 3 :| [4], 2 :| [5], 6 :| [7]] (1 :| [2, 3, 4, 2, 5, 6, 7])
              ]
          ]
