module Tests.Geometry2.TestUtilities where

import Test.Tasty.QuickCheck (counterexample, Property)

import Linear
  ( V2(V2)
  , V3(V3)
  , Metric
  , cross
  , det33
  , distance
  , (*^)
  )

colinear :: V3 Double -> V3 Double -> V3 Double -> Bool
colinear a b c = (det33 $ V3 p q r) < 1e-3
  where
    p = b - a
    q = c - a
    r = cross p q

closeDoubles :: Double -> Double -> Property
closeDoubles a b =
  counterexample (show a ++ " /~ " ++ show b) (a == b || (abs $ a - b) < 1e-6)

closeMetrics :: (Metric f, Floating a, Ord a, Show (f a)) => f a -> f a -> Property
closeMetrics v1 v2 =
  counterexample (show v1 ++ " /~ " ++ show v2) (distance v1 v2 < 1e-6)
