module NaturalNumbersTests (tests) where

import Test.Hspec
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import NaturalNumbers as NN
import Control.Monad
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit

addTest :: Int -> Int -> TestTree
addTest a b = testCase (printf "%d + %d" a b) $
                assertEqual "" (a + b) (toInt (add (fromInt a) (fromInt b)))

mulTest :: Int -> Int -> TestTree
mulTest a b = testCase (printf "%d * %d" a b) $
                assertEqual "" (a * b) (toInt (mul (fromInt a) (fromInt b)))

subTest :: Int -> Int -> TestTree
subTest a b = testCase (printf "%d - %d" a b) $
                assertEqual "" (max 0 (a - b)) (toInt (sub (fromInt a) (fromInt b)))

divTest :: Int -> Int -> TestTree
divTest a b = testCase (printf "%d / %d" a b) $
                assertEqual "" (a `Prelude.div` b) (toInt (NN.div (fromInt a) (fromInt b)))

modTest :: Int -> Int -> TestTree
modTest a b = testCase (printf "%d %% %d" a b) $
                assertEqual "" (a `Prelude.mod` b) (toInt (NN.mod (fromInt a) (fromInt b)))

fromIntTest :: Int -> Nat -> TestTree
fromIntTest a result = testCase (printf "fromInt %d" a) $
                    assertEqual "" (fromInt a) result

toIntTest :: Nat -> Int -> TestTree
toIntTest a result = testCase (printf "toInt x = %d" result) $
                    assertEqual "" (toInt a) result

cmpTest :: Int -> Int -> TestTree
cmpTest a b = testCase (printf "%d <= %d" a b) $
                    assertEqual "" (a <= b) ((fromInt a) <= (fromInt b)) 

evenTest :: Int -> TestTree
evenTest a = testCase (printf "is Even %d" a) $
                assertEqual "" (a `Prelude.mod` 2 == 0) (isEven (fromInt a))


addTests = testGroup "add" $
                  map (\l -> addTest (fst l) (snd l))
                    [(x, y) | x <- [0..10], y <- [0..10]]

mulTests = testGroup "mul" $
                  map (\l -> mulTest (fst l) (snd l))
                    [(x, y) | x <- [0..10], y <- [0..10]]

subTests = testGroup "sub" $
                  map (\l -> subTest (fst l) (snd l))
                    [(x, y) | x <- [0..10], y <- [0..10]]

divTests = testGroup "div" $
                  map (\l -> divTest (fst l) (snd l))
                    [(x, y) | x <- [0,5..100], y <- [1..10]]

modTests = testGroup "mod" $
                  map (\l -> modTest (fst l) (snd l))
                    [(x, y) | x <- [0,5..100], y <- [1..10]]

fromIntTests = testGroup "fromInt" $
                  map (\l -> fromIntTest (fst l) (snd l))
                    [ 
                      (0, Z)
                    , (1, S Z)
                    , (5, S $ S $ S $ S $ S Z)
                    ]

toIntTests = testGroup "toInt" $
                  map (\l -> toIntTest (fst l) (snd l))
                    [ 
                      (Z, 0)
                    , (S Z, 1)
                    , (S $ S $ S $ S $ S Z, 5)
                    ]

cmpTests = testGroup "cmp" $
                  map (\l -> cmpTest (fst l) (snd l))
                    [(x, y) | x <- [0..10], y <- [0..10]]

evenTests = testGroup "isEven" $
                  map (\l -> evenTest l) [0..10]


fromToIntProperties = testGroup "from/toInt properties" 
                          [ 
                            QC.testProperty "identity" $ 
                              (\(NonNegative x) -> toInt (fromInt (x)) == x)
                          ]

eqProperties = testGroup "Eq properties" 
                          [ 
                            QC.testProperty "reflexivity" $ 
                              (\(NonNegative x) -> fromInt x == fromInt x)
                          , QC.testProperty "symmetry" $ 
                              (\(NonNegative (Small x)) (NonNegative (Small y)) -> 
                                let a = fromInt x
                                    b = fromInt y
                                  in (a == b) == (b == a))
                          ]

tests :: TestTree
tests = testGroup "NaturalNumbers"  
          [ testGroup "Unit tests" 
            [
              addTests 
            , mulTests 
            , fromIntTests
            , toIntTests
            , subTests
            , divTests
            , modTests
            , cmpTests
            , evenTests
            ]
          , testGroup "Property tests"
            [ 
              fromToIntProperties 
            , eqProperties
            , QC.testProperty "even isn't odd" $ 
                (\(NonNegative x) -> isEven (fromInt x) /= isOdd (fromInt x))
            , QC.testProperty "even + 1 is odd" $ 
                (\(NonNegative x) -> isEven (fromInt $ x + 1) == isOdd (fromInt x))
            ]
          ]


