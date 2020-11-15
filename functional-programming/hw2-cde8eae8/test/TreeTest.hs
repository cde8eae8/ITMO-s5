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

testApplicative
  :: (Show a, Show b, Eq b) 
  => Tree a 
  -> Tree (a -> b) 
  -> Tree b 
  -> Spec
testApplicative tree fTree exp =
      it (show tree) $ do 
        fTree <*> tree `shouldBe` exp

testTraversable
  :: (Eq a, Eq (b (Tree a)), Show a, Show (b a), Show (b (Tree a)), Applicative b)
  => Tree (b a) 
  -> b (Tree a) 
  -> Spec
testTraversable tree exp =
      it (show tree) $ do 
        sequenceA tree `shouldBe` exp

tests :: IO TestTree
tests = testSpec "" $ do
  let trees = 
        [ Leaf 1
        , Branch (Leaf 1) (Leaf 2)
        , Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
        ]

  describe "Tree" $ do
    describe "Functor" $ do
      testFunctor (2 +) trees
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

    describe "Applicative" $ do
      testApplicative 
        (Leaf 1) 
        (Leaf show)
        (Leaf "1")
      testApplicative 
        (Branch (Leaf 1) (Leaf 2))
        (Branch (Leaf show) (Leaf (\x -> show x ++ "!")))
        (Branch 
            (Branch (Leaf "1") (Leaf "2"))
            (Branch (Leaf "1!") (Leaf "2!")))
      testApplicative 
        (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3)))
        (Branch (Leaf show) (Branch 
          (Leaf (\x -> show x ++ "!"))
          (Leaf (\x -> show x ++ "?"))))
        (Branch 
          (Branch (Leaf "1") (Branch (Leaf "2") (Leaf "3")))
          (Branch 
            (Branch (Leaf "1!") (Branch (Leaf "2!") (Leaf "3!")))
            (Branch (Leaf "1?") (Branch (Leaf "2?") (Leaf "3?")))))
    describe "Traversable" $ do
      testTraversable (Leaf [1]) ([Leaf 1])
      testTraversable 
        (Branch (Leaf [1]) (Leaf [2])) 
        ([Branch (Leaf 1) (Leaf 2)])
      testTraversable 
        (Branch (Leaf $ Just 3) (Leaf $ Just 5))
        (Just $ Branch (Leaf 3) (Leaf 5))
      testTraversable 
        (Branch (Leaf $ Nothing) (Leaf $ Just 5))
        Nothing


