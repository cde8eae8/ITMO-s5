import qualified Criterion.Main as C
import Test.Tasty

import qualified PointTest

main :: IO ()
main = do
  tests <- sequence [ PointTest.tests 
                    ]
  C.defaultMain PointTest.benchmarks
  defaultMain $ testGroup "" tests
