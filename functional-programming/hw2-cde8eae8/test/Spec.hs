import Test.Tasty
import Test.Tasty.HUnit

import qualified ArithmeticTests
import qualified ParserTests
import qualified StringSumTests
import qualified NonEmptyTests
import qualified MovingAverageTests

main :: IO ()
main = do
  tests <- sequence [ ParserTests.tests 
                    , StringSumTests.tests 
                    , NonEmptyTests.tests
                    , ArithmeticTests.tests
                    , MovingAverageTests.tests
                    ]
  defaultMain $ testGroup "" $ tests
  --defaultMain $ testGroup "" $ [ ArithmeticTests.tests, parserTests ]
