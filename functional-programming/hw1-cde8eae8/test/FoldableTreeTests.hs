module FoldableTreeTests (tests) where

import Test.Hspec
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import Data.Foldable
import Data.List
import Control.Monad
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit

import Tree
import FoldableTree

tests :: TestTree
tests = testGroup "FoldableTree"  
          [ testGroup "Tests" 
            [ QC.testProperty "to/fromList" $ 
                (\x -> (toList $ fromList (x :: [Int])) == sort x)
            ]
          ]
