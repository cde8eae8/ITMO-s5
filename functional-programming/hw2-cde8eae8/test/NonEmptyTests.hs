module NonEmptyTests (tests) where

import NonEmpty

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Test.HUnit (assertBool)

testFunctor :: (Show a, Eq a) => NonEmpty Int -> (Int -> a) -> Spec
testFunctor testSet f = it (show testSet) $ 
  (toList $ fmap f testSet) `shouldBe` (fmap f $ toList testSet)

testApplicative :: (Show a, Eq a) => NonEmpty Int -> NonEmpty (Int -> a) -> Spec
testApplicative testSet fs = it (show testSet) $ 
  (toList $ fs <*> testSet) `shouldBe` ((toList fs) <*> (toList testSet))

testMonad :: (Show a, Eq a) => NonEmpty Int -> (Int -> NonEmpty a) -> Spec
testMonad testSet f = it (show testSet) $ 
  (toList $ testSet >>= f) `shouldBe` ((toList testSet) >>= toList . f)

testFoldable :: (Show a, Eq a, Monoid a) => NonEmpty Int -> (Int -> a) -> Spec
testFoldable testSet f = it (show testSet) $ 
  (foldMap f testSet) `shouldBe` (foldMap f (toList testSet))

testTraversable :: (Show b, Show (f a), Show (f [a]), Eq (f [a]), Applicative f) => NonEmpty b -> (b -> f a) -> Spec
testTraversable testSet f = it (show testSet) $ 
  (fmap toList $ traverse f testSet) `shouldBe` (traverse f (toList testSet))

tests :: IO TestTree
tests = testSpec "" $ do
  let xs = [ (1 :| [])
           , (1 :| [2])
           , (1 :| [2, 3, 4])
           ]

  describe "to/from list" $ do
    it "fromList#1" $ fromList [1] `shouldBe` (1 :| [])
    it "fromList#2" $ fromList [1, 2, 3] `shouldBe` (1 :| [2, 3])
    it "toList#1"   $ [1] `shouldBe` toList (1 :| [])
    it "toList#2"   $ [1, 2, 3] `shouldBe` toList (1 :| [2, 3])

  describe "Functor" $ do
    sequence_ [ testFunctor x show | x <- xs ]

  describe "Applicative" $ do
    let fs = [ (show :| [])
             , (show :| [\x -> show $ x * x])
             , (show :| [\x -> show $ x * x, const "123"])
             ]
    it "pure" $ pure 1 `shouldBe` (1 :| [])
    sequence_ [ testApplicative x f | f <- fs, x <- xs ]

  describe "Monad" $ do
    let fs = [ show, \x -> show $ x + 1, const "1" ]
    it "return" $ return 1 `shouldBe` (1 :| [])
    sequence_ [ testMonad x (fromList . f) | x <- xs, f <- fs ]

  describe "Foldable" $ do
    sequence_ [ testFoldable x show | x <- xs ]

  describe "Traversable" $ do
    sequence_ [ testTraversable x show | x <- xs ]

