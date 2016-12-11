import Test.Tasty

import Tests.Geometry2.Geodesic (tests)
import Tests.Geometry2.Primitives (tests)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Tests.Geometry2.Geodesic.tests
  , Tests.Geometry2.Primitives.tests
  ]
