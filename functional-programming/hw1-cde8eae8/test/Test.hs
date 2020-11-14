module Main where
import Test.Tasty
import Test.Tasty.HUnit

import qualified WeekdaysTests 
import qualified NaturalNumbersTests
import qualified TreeTests
import qualified SplitTests
import qualified FoldableTreeTests
import qualified MonoidsTests

main = defaultMain $ testGroup "" $ 
  [ WeekdaysTests.tests
  , NaturalNumbersTests.tests
  , TreeTests.tests
  , FoldableTreeTests.tests
  , SplitTests.tests
  , FoldableTreeTests.tests
  , MonoidsTests.tests
  ]
