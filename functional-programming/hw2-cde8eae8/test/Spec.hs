import Test.Tasty
import Test.Tasty.HUnit

import qualified ArithmeticTest
import qualified ParserTest
import qualified StringSumTest
import qualified NonEmptyTest
import qualified MovingAverageTest

main :: IO ()
main = do
  tests <- sequence [ ParserTest.tests 
                    , StringSumTest.tests 
                    , NonEmptyTest.tests
                    , ArithmeticTest.tests
                    , MovingAverageTest.tests
                    ]
  defaultMain $ testGroup "" $ tests
