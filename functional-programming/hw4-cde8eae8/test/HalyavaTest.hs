module HalyavaTest
  ( tests
  ) where

import Data.List
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Halyava
import HalyavaShow

tests :: IO TestTree
tests = testSpec "" $ do
  describe "Base operations" $ do
    it "fib 4" $ runShow (fibNumbers 4) `shouldBe` expected
      where 
        expected = intercalate "\n"
          [ "var v0 = 0.0"
          , "var v1 = 1.0"
          , "var v2 = 0.0"
          , "while (v2<=4.0) {"
          , "  var v3 = v0+v1"
          , "  v0 = v1"
          , "  v1 = v3"
          , "  v2 = v2+1.0"
          , "}"
          , "var v3 = \"!\""
          , "console.log((4.0+ \"\")+\"-th fibonacci number is \"+(v0+ \"\")+v3)"
          , "var v3 = v2>=10.0"
          , "var v4 = True"
          , "console.log((v3+ \"\")+\" && \"+(v4+ \"\")+\" = \"+(v3&&v4+ \"\"))"
          ]
