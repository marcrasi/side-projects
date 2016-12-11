module Tests.Geometry2.Geodesic where

import Test.Tasty
import Test.Tasty.QuickCheck

import Linear
  ( V2(V2)
  , V3(V3)
  , Metric
  , cross
  , det33
  , distance
  , dot
  , norm
  , normalize
  , (*^)
  , (!*)
  )

import Instances.Linear ()

import Tests.Geometry2.TestUtilities
import Geometry2.DiscreteSurfaces (square)
import Geometry2.Geodesic
import Geometry2.Primitives

tests = testGroup "Geodesic"
  [ testGroup "perp"
    [ testProperty "perp_perp" perp_perp
    ]
  , testGroup "distanceToExitHalfplane"
    [ testProperty "distanceToExitHalfplane_otherSide_infinity" distanceToExitHalfplane_otherSide_infinity
    , testProperty "distanceToExitHalfplane_parallel_infinity" distanceToExitHalfplane_parallel_infinity
    , testProperty "distanceToExitHalfplane_away_infinity" distanceToExitHalfplane_away_infinity
    , testProperty "distanceToExitHalfplane_axisAligned" distanceToExitHalfplane_axisAligned
    , testProperty "distanceToExitHalfplane_translationInvariant" distanceToExitHalfplane_translationInvariant
    , testProperty "distanceToExitHalfplane_rotationInvariant" distanceToExitHalfplane_rotationInvariant
    ]
  , testGroup "advanceGeodesic"
    [ testProperty "advanceGeodesic_square" advanceGeodesic_square
    ]
  ]

perp_perp v = dot (perp v) v < 1e-6

{-
 - Whether v1 and v2 are on the same side of the a-b line.
 -}
onSameSide :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Bool
onSameSide a b v1 v2 =
  (dot (v1 - a) perpToLine) * (dot (v2 - a) perpToLine) > 0
  where
    perpToLine = perp $ b - a

{-
 - Whether the vector `direction` at position `start` is pointing away from
 - the a-b line.
 -}
away :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Bool
away a b start direction =
  (dot (start - a) perpToLine) * (dot direction perpToLine) > 0
  where
    perpToLine = perp $ b - a

distanceToExitHalfplane_otherSide_infinity a b c otherSide direction =
  (not $ onSameSide a b c otherSide) ==>
    distanceToExitHalfplane a b c otherSide direction === read "Infinity"

distanceToExitHalfplane_parallel_infinity a b c start =
  (onSameSide a b c start) ==>
    distanceToExitHalfplane a b c start (b - a) > 1e10

distanceToExitHalfplane_away_infinity a b c start direction =
  (onSameSide a b c start && away a b start direction) ==>
    distanceToExitHalfplane a b c start direction === read "Infinity"

distanceToExitHalfplane_axisAligned startX =
  (startX > 0) ==>
    closeDoubles
      (distanceToExitHalfplane (V2 0 0) (V2 0 1) (V2 1 0) (V2 startX 0) (V2 (-1) 0))
      startX

distanceToExitHalfplane_translationInvariant a b c start direction t =
  closeDoubles
    (distanceToExitHalfplane a b c start direction)
    (distanceToExitHalfplane (a + t) (b + t) (c + t) (start + t) direction)

distanceToExitHalfplane_rotationInvariant a b c start direction angle =
  closeDoubles
    (distanceToExitHalfplane a b c start direction)
    (distanceToExitHalfplane (rot !* a) (rot !* b) (rot !* c) (rot !* start) (rot !* direction))
  where
    rot = V2 (V2 (cos angle) (sin angle)) (V2 (-(sin angle)) (cos angle))

data SquareCase = SquareCase Vec2 Vec2 Double
  deriving (Show)

instance (Arbitrary SquareCase) where
  arbitrary = SquareCase <$>
    (V2 <$> choose (0, 1) <*> choose (0, 1)) <*>
    (fmap normalize arbitrary) <*>
    choose (0, 1)

advanceGeodesic_square (SquareCase start direction distance) =
  ((inSquare start) && (inSquare expectedResult)) ==>
    closeMetrics expectedResult ambientResult2
  where
    inSquare (V2 x y) = x >= 0 && y >= 0 && x <= 1 && y <= 1

    V2 startX startY = start
    start3 = V3 startX startY 0
    V2 directionX directionY = direction
    direction3 = V3 directionX directionY 0

    -- TODO: Make a general function that converts a point in ambient
    -- coordinates to the closest surface point.
    surfaceStartFace = if startY < startX
      then FaceIndex 0
      else FaceIndex 1
    surfaceStartCoordinateTransform = lookupCoordinateTransform (coordinateTransforms square) surfaceStartFace
    surfaceStart = SurfaceCoordinate
      surfaceStartFace
      (toFace surfaceStartCoordinateTransform start3)

    -- hacky but correct way of turning an ambient direction into
    -- face-coordinates
    -- TODO: General direction mapping functions.
    faceDirection = toFace surfaceStartCoordinateTransform direction3 -
      toFace surfaceStartCoordinateTransform (V3 0 0 0)

    surfaceResult = advanceGeodesic square surfaceStart faceDirection distance
    ambientResult = surfaceToAmbient square surfaceResult
    V3 ambientResultX ambientResultY _ = ambientResult
    ambientResult2 = V2 ambientResultX ambientResultY

    expectedResult = start + distance *^ direction

