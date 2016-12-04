module Tests.Geometry2.Primitives where

import Test.Tasty
import Test.Tasty.QuickCheck

import Linear
  ( V2(V2)
  , V3(V3)
  , Metric
  , cross
  , det33
  , distance
  , (*^)
  )

import Instances.Linear ()

import Geometry2.Primitives

tests = testGroup "Primitives"
  [ testGroup "canonicalize"
    [ testProperty "canonicalize_origin" canonicalize_origin
    , testProperty "canonicalize_posXAxis" canonicalize_posXAxis
    , testProperty "canonicalize_posYPlane" canonicalize_posYPlane
    , testProperty "canonicalize_preservesDistance" canonicalize_preservesDistance
    ]
  , testGroup "canonicalizeFace"
    [ testProperty "canonicalizeFace_correctlyBounded" canonicalizeFace_correctlyBounded
    ]
  ]

colinear :: V3 Double -> V3 Double -> V3 Double -> Bool
colinear a b c = (det33 $ V3 p q r) < 0.001
  where
    p = b - a
    q = c - a
    r = cross p q

canonicalize_origin origin posXAxis posYPlane =
  (not $ colinear origin posXAxis posYPlane) ==>
    canonicalize origin posXAxis posYPlane origin === V2 0 0

canonicalize_posXAxis origin posXAxis posYPlane =
  (not $ colinear origin posXAxis posYPlane) ==>
    closeMetrics
      (canonicalize origin posXAxis posYPlane posXAxis)
      (V2 (distance origin posXAxis) 0)

canonicalize_posYPlane origin posXAxis posYPlane =
  (not $ colinear origin posXAxis posYPlane) ==> y > 0
  where
    V2 x y = canonicalize origin posXAxis posYPlane posYPlane

canonicalize_preservesDistance a b c ta1 tb1 ta2 tb2 =
  (not $ colinear a b c) ==> closeDoubles originalDistance transformedDistance
  where
    v1 = ta1 *^ a + tb1 *^ b + (1 - ta1 - tb1) *^ c
    v2 = ta2 *^ a + tb2 *^ b + (1 - ta2 - tb2) *^ c
    originalDistance = distance v1 v2
    transformedDistance = distance (canonicalize a b c v1) (canonicalize a b c v2)

-- TODO: need to rethink where I can do exact comparisons and where not,
-- because I want to actually be sure that 0 <= x2 <= x1
canonicalizeFace_correctlyBounded a b c =
  (not $ colinear a b c) ==>
    conjoin
      [ counterexample ("x1: " ++ show x1 ++ " < 0") (x1 >= (-epsilon))
      , counterexample ("x2: " ++ show x2 ++ " < 0") (x2 >= (-epsilon))
      , counterexample ("x3: " ++ show x3 ++ " < 0") (x3 >= (-epsilon))
      , counterexample ("y1: " ++ show y1 ++ " < 0") (y1 >= (-epsilon))
      , counterexample ("y2: " ++ show y2 ++ " < 0") (y2 >= (-epsilon))
      , counterexample ("y3: " ++ show y3 ++ " < 0") (y3 >= (-epsilon))
      , counterexample ("x1: " ++ show x1 ++ " > xmax (" ++ show xmax ++ ")") (x1 <= xmax + epsilon)
      , counterexample ("x2: " ++ show x2 ++ " > xmax (" ++ show xmax ++ ")") (x2 <= xmax + epsilon)
      , counterexample ("x3: " ++ show x3 ++ " > xmax (" ++ show xmax ++ ")") (x3 <= xmax + epsilon)
      ]
  where
    epsilon = 0.00001
    face = EmbeddedFace (Vertex a) (Vertex b) (Vertex c)
    V2 x1 y1 = canonicalizeFace face a
    V2 x2 y2 = canonicalizeFace face b
    V2 x3 y3 = canonicalizeFace face c
    (xmax, _) = head $ dropWhile (not . (\(x, y) -> x > 0 && y < epsilon)) [(x1, y1), (x2, y2), (x3, y3)]

closeDoubles :: Double -> Double -> Property
closeDoubles a b =
  counterexample (show a ++ " /~ " ++ show b) ((abs $ a - b) < 0.001)

closeMetrics :: (Metric f, Floating a, Ord a, Show (f a)) => f a -> f a -> Property
closeMetrics v1 v2 =
  counterexample (show v1 ++ " /~ " ++ show v2) (distance v1 v2 < 0.001)
