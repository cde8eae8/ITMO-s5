import Test.Tasty
import Test.Tasty.HUnit

import qualified ArithmeticTest
import qualified NonEmptyTest
import qualified MovingAverageTest
import qualified TreeTest
import qualified ParserTest
import qualified StringSumTest

main :: IO ()
main = do
  tests <- sequence [ StringSumTest.tests 
                    , TreeTest.tests
                    , NonEmptyTest.tests
                    , ArithmeticTest.tests
                    , MovingAverageTest.tests
                    , ParserTest.tests 
                    ]
  defaultMain $ testGroup "" $ tests
