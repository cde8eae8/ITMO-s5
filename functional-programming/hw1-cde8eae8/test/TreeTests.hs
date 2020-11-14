module TreeTests (tests) where

import Test.Hspec
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import Tree 
import Control.Monad
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe
import qualified Data.List.NonEmpty as NN

isBinaryTree :: (Ord a) => Tree a -> Bool
isBinaryTree Leaf                            = True
isBinaryTree (Node xs Leaf Leaf)             = True
isBinaryTree (Node xs Leaf r@(Node rxs _ _)) = (NN.head xs) < (NN.head rxs) && isBinaryTree r
isBinaryTree (Node xs l@(Node lxs _ _) Leaf) = (NN.head xs) > (NN.head lxs) && isBinaryTree l
isBinaryTree (Node xs l@(Node lxs _ _) r@(Node rxs _ _)) = 
              lkey < key && key < rkey && isBinaryTree l && isBinaryTree r
              where lkey = NN.head lxs
                    key  = NN.head xs
                    rkey = NN.head rxs

isEmptyTest :: [Int] -> Bool -> TestTree
isEmptyTest list result = testCase (show list) $
                        assertEqual "" result (isEmpty (fromList list))

sizeTest :: [Int] -> TestTree
sizeTest list = testCase (show list) $
                        assertEqual "" (length list) (size (fromList list))

findTest :: [Int] -> Int -> TestTree
findTest list elem = testCase (show list) $
                      assertEqual "" 
                        (filter ((==) elem) list) 
                        (fromMaybe [] $ fmap NN.toList $ (find (fromList list) elem))

insertTest :: [Int] -> [Int] -> TestTree
insertTest initList elemsList = testCase (show initList ++ " " ++ show elemsList) $
                                  assertEqual "" (fromList $ initList ++ elemsList)
                                                 (foldl insert tree elemsList)
                                    where tree = fromList initList

fromListTest :: [Int] -> TestTree
fromListTest list = testCase (show list) $
                        assertBool "" $ isBinaryTree (fromList list) 

eraseTestChecker :: (Ord a) => (Tree a, Bool) -> a -> (Tree a, Bool)
eraseTestChecker (t, False) _   = (t, False)
eraseTestChecker (t, True) elem = (newTree, isBinaryTree newTree && (not $ contains elem newTree))
  where newTree = (erase t elem)

eraseTest :: [Int] -> [Int] -> TestTree
eraseTest initList eraseList = testCase (show initList ++ " " ++ show eraseList) $
                        assertBool "" $ 
                          (snd (foldl eraseTestChecker ((fromList initList), True) eraseList))

isEmptyTests :: TestTree
isEmptyTests = testGroup "isEmpty" $
                  [ isEmptyTest []              True
                  , isEmptyTest [1]             False 
                  , isEmptyTest [1, 1, 1, 1, 1] False 
                  , isEmptyTest [1, 2, 3]       False
                  ]

sizeTests :: TestTree
sizeTests = testGroup "size" $
                  [ sizeTest []                 
                  , sizeTest [1]               
                  , sizeTest [1, 1, 1, 1, 1, 1]
                  , sizeTest [1, 2, 3]          
                  ]

findTests :: TestTree
findTests = testGroup "find" $
                  [ findTest [1]                1
                  , findTest [1, 1, 1, 1, 1, 1] 1
                  , findTest [1, 1, 1, 1, 1, 1] 2
                  , findTest [1, 2, 3]          1
                  , findTest [1, 2, 3]          2
                  , findTest [1, 2, 3]          3
                  , findTest [1, 2, 3]          0
                  ]

insertTests :: TestTree
insertTests = testGroup "find" $
                  [ insertTest [1]    [1..5]               
                  , insertTest [1..5] [1..5]               
                  , insertTest [1..5] [1..10]               
                  , insertTest [] [1..10]               
                  ]

fromListTests :: TestTree
fromListTests = testGroup "fromList" $
                  [ fromListTest  []
                  , fromListTest [1]
                  , fromListTest [1..10]
                  , fromListTest [10,9..1]
                  , fromListTest [1, 5, 1, 2, 3, 4, 7, 1, 1, 4, 9, 5, 3, 7]
                  ]

eraseTests :: TestTree
eraseTests = testGroup "fromList" $
                  [ eraseTest  [] [1..6]
                  , eraseTest [1..6] [1..6]
                  , eraseTest [1..10] [10,9..1]
                  , eraseTest [10,9..1] [1..10]
                  , eraseTest [1, 5, 1, 2, 3, 4, 7, 1, 1, 4, 9, 5, 3, 7] 
                              [6, 2, 3, 5, 6, 6, 6, 6, 6, 7, 8]
                  ]

tests :: TestTree
tests = testGroup "Tree" 
          [ isEmptyTests
          , sizeTests
          , findTests
          , insertTests 
          , eraseTests 
          , fromListTests 
          ] 
