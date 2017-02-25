module Tests.Geometry2.Primitives where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Vector (Vector)
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
import Geometry2.DiscreteSurfaces (square)
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
  , testGroup "bucketCenter"
    [ testProperty "bucketCenter_inverse_1" bucketCenter_inverse_1
    , testProperty "bucketCenter_inverse_2" bucketCenter_inverse_2
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

data InSquare = InSquare (V2 Double)
  deriving (Show)

instance (Arbitrary InSquare) where
  arbitrary = InSquare <$> (V2 <$> choose (0, 1) <*> choose (0, 1))

data GoodFineness = GoodFineness Double
  deriving (Show)

instance (Arbitrary GoodFineness) where
  arbitrary = GoodFineness <$> choose (0.01, 0.5)

bucketCenter_inverse_1 (InSquare ambientStart) (GoodFineness fineness) =
  case mayBucketCenterAmbient of
    Just (V3 startBucketCenterX startBucketCenterY _) ->
      counterexample
        ("V2 " ++ show startBucketCenterX ++ " " ++ show startBucketCenterY ++ " not within fineness of " ++ show ambientStart) $
        conjoin
          [ abs (x - startBucketCenterX) <= fineness
          , abs (y - startBucketCenterY) <= fineness
          ]
    Nothing -> counterexample "Didn't get a bucket center." False
  where
    field = replicateSurfaceField fineness 0 square :: SurfaceField Vector Double

    -- TODO: Make a general function that converts a point in ambient
    -- coordinates to the closest surface point.
    V2 x y = ambientStart
    ambientStart3 = V3 x y 0
    surfaceStartFace = if y < x
      then FaceIndex 0
      else FaceIndex 1
    surfaceStartCoordinateTransform = lookupCoordinateTransform (coordinateTransforms square) surfaceStartFace
    surfaceStart = SurfaceCoordinate
      surfaceStartFace
      (toFace surfaceStartCoordinateTransform ambientStart3)

    startSurfaceBucketCoordinate = toSurfaceBucketCoordinate field surfaceStart

    mayStartBucketCenter = bucketCenter square field startSurfaceBucketCoordinate

    mayBucketCenterAmbient = fmap (\(SurfaceCoordinate _ v) -> toAmbient surfaceStartCoordinateTransform v) mayStartBucketCenter

bucketCenter_inverse_2 faceIndexInt faceBucketIndexInt (GoodFineness fineness) =
  counterexample debugInfo $ case maySurfaceBucketCoordinate of
    Just surfaceBucketCoordinate -> surfaceBucketCoordinate === start
    Nothing -> 1 === 1 -- TODO: find cleaner way of always passing here
  where
    field = replicateSurfaceField fineness 0 square :: SurfaceField Vector Double

    faceIndex = FaceIndex (faceIndexInt `mod` 2)
    faceBucketIndex = FaceBucketIndex (faceBucketIndexInt `mod` (bucketCount $ lookupFaceField field faceIndex))

    start = SurfaceBucketCoordinate faceIndex faceBucketIndex
    mayStartCenter = bucketCenter square field start
    maySurfaceBucketCoordinate = fmap (\startCenter -> toSurfaceBucketCoordinate field startCenter) mayStartCenter

    debugInfo =
      "start = " ++ show start ++ "\n" ++
        "mayStartCenter = " ++ show mayStartCenter ++ "\n"

