module StringSumTest (tests) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import StringSum

test :: String -> Maybe Int -> Spec
test str exp = do 
  it (show str) $ (stringSum str) `shouldBe` exp

tests :: IO TestTree
tests = testSpec "" $ do
  describe "StringSum" $ do
    it "one number" $ 
      stringSum "1" `shouldBe` Just 1
    it "one number with spaces" $ 
      stringSum "  1  " `shouldBe` Just 1
    describe "numbers" $ do
      test "1 2 3 4 5 6 7 8" $ Just 36
      test "   1 2   3 4 5   6 7 8   " $ Just 36
      test "   -1 2   3 -4 5   6 7 -8   " $ Just 10
    describe "bad strings" $ do
      test "  sjd -1 2   3 -4 5   6 7 -8   "  $ Nothing 
      test "  -1sad 2   3 -4 5   6 7 -8   "   $ Nothing 
      test " a -1 2   3 -4 5   6 7 -8   "     $ Nothing 
      test "  -1 2   3 -4 5 a 6 7 -8   "      $ Nothing 
      test "  -1 2   3 -4 56a 7 -8   "        $ Nothing 
      test "  -1 2   a3 -4 56a 7 -8   "       $ Nothing 
      test "  -1 2   3 -4 56 7 -8a"           $ Nothing 


