import qualified Criterion.Main as C
import Test.Tasty

import qualified PointTest
import qualified HalyavaTest

main :: IO ()
main = do
  tests <- sequence [ PointTest.tests 
                    , HalyavaTest.tests
                    ]
  C.defaultMain PointTest.benchmarks
  defaultMain $ testGroup "" tests
