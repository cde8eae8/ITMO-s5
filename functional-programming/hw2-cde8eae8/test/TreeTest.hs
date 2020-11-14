module TreeTest (tests) where

import Control.Monad
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Text.Printf

import Tree

testFunctor 
  :: (Show a, Eq b, Show b)
  => (a -> b) 
  -> [Tree a] 
  -> [Tree b] 
  -> Spec
testFunctor f testSets expected = do
    mapM_ (\(set, exp) -> 
      it (show set) $ do 
        fmap f set `shouldBe` exp) (zip testSets expected)

testFoldable 
  :: (Show a, Eq b, Monoid b, Show b) 
  => (a -> b) 
  -> [Tree a]
  -> [b]
  -> Spec
testFoldable f testSets expected = do
    mapM_ (\(set, exp) -> 
      it (show set) $ do 
        foldMap f set `shouldBe` exp) (zip testSets expected)
  --it (show testSet) $ do 
  --  foldMap f testSet `shouldBe` expected

tests :: IO TestTree
tests = testSpec "" $ do
  let trees = 
        [ Leaf 1
        , Branch (Leaf 1) (Leaf 2)
        , Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
        ]
  describe "Tree" $ do
    describe "Functor" $ do
      testFunctor (\x -> x + 2) trees  
        [ Leaf 3
        , Branch (Leaf 3) (Leaf 4)
        , Branch (Leaf 3) (Branch (Leaf 4) (Leaf 5))
        ]

    describe "Foldable" $ do
      testFoldable (\x -> show x) trees  
        [ "1"
        , "12"
        , "123"
        ]


