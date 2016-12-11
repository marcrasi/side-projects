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

import Tests.Geometry2.TestUtilities
import Geometry2.Primitives

tests = testGroup "Primitives"
  [ testGroup "makeCoordinateTransform"
    [ testProperty "makeCoordinateTransform_inverse_1" makeCoordinateTransform_inverse_1
    , testProperty "makeCoordinateTransform_inverse_2" makeCoordinateTransform_inverse_2
    , testProperty "makeCoordinateTransform_toFace_origin" makeCoordinateTransform_toFace_origin
    , testProperty "makeCoordinateTransform_toFace_posXAxis" makeCoordinateTransform_toFace_posXAxis
    , testProperty "makeCoordinateTransform_toFace_posYPlane" makeCoordinateTransform_toFace_posYPlane
    , testProperty "makeCoordinateTransform_toFace_isometric" makeCoordinateTransform_toFace_isometric
    ]
  , testGroup "canonicalCoordinateTransform"
    [ testProperty "canonicalCoordinateTransform_correctlyBounded" canonicalCoordinateTransform_correctlyBounded
    ]
  ]

onFacePlane :: Vec3 -> Vec3 -> Vec3 -> Double -> Double -> Vec3
onFacePlane a b c s t = s *^ (b - a) + t *^ (c - a) + a

makeCoordinateTransform' a b c =
  makeCoordinateTransform
    (IndexedVertex (VertexIndex 0) (Vertex a))
    (IndexedVertex (VertexIndex 1) (Vertex b))
    (IndexedVertex (VertexIndex 2) (Vertex c))

makeCoordinateTransform_inverse_1 origin posXAxis posYPlane s t =
  (not $ colinear origin posXAxis posYPlane) ==>
    closeMetrics (toAmbient transform $ toFace transform v) v
  where
    transform = makeCoordinateTransform' origin posXAxis posYPlane
    v = onFacePlane origin posXAxis posYPlane s t

makeCoordinateTransform_inverse_2 origin posXAxis posYPlane v =
  (not $ colinear origin posXAxis posYPlane) ==>
    closeMetrics (toFace transform $ toAmbient transform v) v
  where
    transform = makeCoordinateTransform' origin posXAxis posYPlane

makeCoordinateTransform_toFace_origin origin posXAxis posYPlane =
  (not $ colinear origin posXAxis posYPlane) ==>
    closeMetrics (toFace transform origin) (V2 0 0)
  where
    transform = makeCoordinateTransform' origin posXAxis posYPlane

makeCoordinateTransform_toFace_posXAxis origin posXAxis posYPlane =
  (not $ colinear origin posXAxis posYPlane) ==>
    closeMetrics (toFace transform posXAxis) (V2 (distance origin posXAxis) 0)
  where
    transform = makeCoordinateTransform' origin posXAxis posYPlane

makeCoordinateTransform_toFace_posYPlane origin posXAxis posYPlane =
  (not $ colinear origin posXAxis posYPlane) ==> y > 0
  where
    transform = makeCoordinateTransform' origin posXAxis posYPlane
    V2 _ y = toFace transform posYPlane

makeCoordinateTransform_toFace_isometric origin posXAxis posYPlane s1 t1 s2 t2 =
  (not $ colinear origin posXAxis posYPlane) ==>
    closeDoubles (distance (toFace transform v1) (toFace transform v2)) (distance v1 v2)
  where
    transform = makeCoordinateTransform' origin posXAxis posYPlane
    v1 = onFacePlane origin posXAxis posYPlane s1 t1
    v2 = onFacePlane origin posXAxis posYPlane s2 t2

makeCoordinateTransform_toAmbient_isometric origin posXAxis posYPlane v1 v2 =
  (not $ colinear origin posXAxis posYPlane) ==>
    closeDoubles (distance (toAmbient transform v1) (toAmbient transform v2)) (distance v1 v2)
  where
    transform = makeCoordinateTransform' origin posXAxis posYPlane

canonicalCoordinateTransform_correctlyBounded a b c =
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
    epsilon = 1e-6
    face =
      EmbeddedFace
        (IndexedVertex (VertexIndex 0) (Vertex a))
        (IndexedVertex (VertexIndex 1) (Vertex b))
        (IndexedVertex (VertexIndex 2) (Vertex c))
    transform = canonicalCoordinateTransform face
    V2 x1 y1 = toFace transform a
    V2 x2 y2 = toFace transform b
    V2 x3 y3 = toFace transform c
    (xmax, _) = head $ dropWhile (not . (\(x, y) -> x > 0 && y < epsilon)) [(x1, y1), (x2, y2), (x3, y3)]


