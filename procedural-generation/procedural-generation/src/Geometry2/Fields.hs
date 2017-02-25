module Geometry2.Fields where

{-
 - A discrete field of quantities of type `a` on a face.
 -
 - The fineness is the side length of the (square) bucket, in ambient
 - space.
 -
 - Position (x, y) lives inside bucket
 - (bx, by) = (floor $ x / fineness, floor $ y / fineness):
 -
 -
 -      |<-----fineness----->|
 -      o--------------------o -
 -      |                    | ^
 -      |                    | |
 -      |                    | |
 -      |     * (x, y)       | fineness
 -      |                    | |
 -      |                    | |
 -      |                    | |
 -      |                    | v
 -      o--------------------o -
 -
 - Bucket (bx, by) is element (bx + xBucketCount * by) of the `values`
 - Vector.
 -
 - There are (xBucketCount * yBucketCount) buckets.
 -
 - When a bucket is partially or fully outside of the face, "full bucket"
 - refers to the entire square and "bucket" refers to the intersection
 - between the full bucket and the face.
 -
 - TODO: Make `values` be an unboxed Vector.
 -}
data FaceField v a = FaceField
  { values :: v a
  , fineness :: Double
  , xBucketCount :: Int
  , yBucketCount :: Int
  }

bucketCount :: FaceField v a -> Int
bucketCount (FaceField _ _ xBucketCount yBucketCount) =
  xBucketCount * yBucketCount

createFaceField :: GV.Vector v a => Double -> a -> a -> CoordinateTransform -> FaceField v a
createFaceField fineness initialValue iv2 coordinateTransform =
  FaceField
    { values = GV.generate (xBucketCount * yBucketCount) (\i ->
      if (i `mod` xBucketCount) `mod` 2 == (i `div` xBucketCount) `mod` 2
        then initialValue
        else iv2)
    , fineness = fineness
    , xBucketCount = xBucketCount
    , yBucketCount = yBucketCount
    }
  where
    V2 xMax _ = faceVertex1 coordinateTransform
    V2 _ yMax = faceVertex2 coordinateTransform
    xBucketCount = ceiling $ xMax / fineness
    yBucketCount = ceiling $ yMax / fineness

replicateFaceField :: GV.Vector v a => Double -> a -> CoordinateTransform -> FaceField v a
replicateFaceField fineness initialValue coordinateTransform =
  FaceField
    { values = GV.generate (xBucketCount * yBucketCount) (\_ -> initialValue)
    , fineness = fineness
    , xBucketCount = xBucketCount
    , yBucketCount = yBucketCount
    }
  where
    V2 xMax _ = faceVertex1 coordinateTransform
    V2 _ yMax = faceVertex2 coordinateTransform
    xBucketCount = ceiling $ xMax / fineness
    yBucketCount = ceiling $ yMax / fineness

data FaceBucketIndex = FaceBucketIndex Int
  deriving (Show, Eq)

toFaceBucketIndex :: FaceField v a -> Vec2 -> FaceBucketIndex
toFaceBucketIndex (FaceField _ fineness xBucketCount _) (V2 x y) =
  FaceBucketIndex $ (floor $ x / fineness) + xBucketCount * (floor $ y / fineness)

bucketX :: FaceField v a -> FaceBucketIndex -> Int
bucketX (FaceField _ _ xBucketCount _) (FaceBucketIndex faceBucketIndex) =
  faceBucketIndex `mod` xBucketCount

bucketY :: FaceField v a -> FaceBucketIndex -> Int
bucketY (FaceField _ _ xBucketCount _) (FaceBucketIndex faceBucketIndex) =
  faceBucketIndex `div` xBucketCount

fullBucketBottomY :: FaceField v a -> FaceBucketIndex -> Double
fullBucketBottomY faceField faceBucketIndex =
  (fineness faceField) * (fromIntegral $ bucketY faceField faceBucketIndex)

fullBucketTopY :: FaceField v a -> FaceBucketIndex -> Double
fullBucketTopY faceField faceBucketIndex =
  (fineness faceField) * (1 + (fromIntegral $ bucketY faceField faceBucketIndex))

fullBucketLeftX :: FaceField v a -> FaceBucketIndex -> Double
fullBucketLeftX faceField faceBucketIndex =
  (fineness faceField) * (fromIntegral $ bucketX faceField faceBucketIndex)

fullBucketRightX :: FaceField v a -> FaceBucketIndex -> Double
fullBucketRightX faceField faceBucketIndex =
  (fineness faceField) * (1 + (fromIntegral $ bucketX faceField faceBucketIndex))

fullBucketBottomLeft :: FaceField v a -> FaceBucketIndex -> Vec2
fullBucketBottomLeft faceField faceBucketIndex = V2
  (fullBucketLeftX faceField faceBucketIndex)
  (fullBucketBottomY faceField faceBucketIndex)

fullBucketTopRight :: FaceField v a -> FaceBucketIndex -> Vec2
fullBucketTopRight faceField faceBucketIndex = V2
  (fullBucketRightX faceField faceBucketIndex)
  (fullBucketTopY faceField faceBucketIndex)

{-
 - One FaceField per face.
 -}
data SurfaceField v a = SurfaceField (Vector (FaceField v a))

createSurfaceField :: GV.Vector v a => Double -> a -> a -> DiscreteSurface -> SurfaceField v a
createSurfaceField fineness initialValue iv2 surface = SurfaceField $
  fromList $
  map (createFaceField fineness initialValue iv2) $
  toList $
  coordinateTransforms surface

replicateSurfaceField :: GV.Vector v a => Double -> a -> DiscreteSurface -> SurfaceField v a
replicateSurfaceField fineness initialValue surface = SurfaceField $
  fromList $
  map (replicateFaceField fineness initialValue) $
  toList $
  coordinateTransforms surface

data SurfaceBucketCoordinate = SurfaceBucketCoordinate
  { bFace :: FaceIndex
  , faceBucketIndex :: FaceBucketIndex
  }
  deriving (Show, Eq)

toSurfaceBucketCoordinate :: SurfaceField v a -> SurfaceCoordinate -> SurfaceBucketCoordinate
toSurfaceBucketCoordinate surfaceField (SurfaceCoordinate faceIndex faceCoordinate) =
  SurfaceBucketCoordinate
    faceIndex
    (toFaceBucketIndex (lookupFaceField surfaceField faceIndex) faceCoordinate)

{-
 - Returns a sample of points that are in the given bucket.
 -
 - The number of samples is distributed Possion(A * n), where A is the size
 - of the bucket relative to the full-size bucket.
 -}
sampleBucket :: (RandomGen g) => DiscreteSurface -> Int -> SurfaceBucketCoordinate -> Rand g [SurfaceCoordinate]
sampleBucket surface n bucket = return []

{-
 - So the general shape of a bucket is a (possibly degenerate)
 - quadrilateral. "All" I need to do is to find the four corners and then
 - derive some quadrilateral center equation thing!
 -}
bucketCenter :: DiscreteSurface -> SurfaceField v a -> SurfaceBucketCoordinate -> Maybe SurfaceCoordinate
bucketCenter surface surfaceField (SurfaceBucketCoordinate bFace faceBucketIndex) =
  if nonEmptyBucketFaceIntersection
    then Just $ SurfaceCoordinate bFace $ V2 centerX centerY
    --then Just $ SurfaceCoordinate bFace $ V2 (fullLeftX + (fineness faceField) / 2) (fullBottomY + (fineness faceField) / 2)
    else Nothing
  where
    coordinateTransform = lookupCoordinateTransform (coordinateTransforms surface) bFace
    V2 faceMaxX _ = faceVertex1 coordinateTransform
    V2 faceTipX faceTipY = faceVertex2 coordinateTransform

    faceField = lookupFaceField surfaceField bFace
    fullBottomY = fullBucketBottomY faceField faceBucketIndex
    fullTopY = fullBucketTopY faceField faceBucketIndex
    fullLeftX = fullBucketLeftX faceField faceBucketIndex
    fullRightX = fullBucketRightX faceField faceBucketIndex

    faceBottomLeftX = (faceTipX * bottomY / faceTipY)
    faceBottomRightX = ((faceTipX - faceMaxX) * bottomY / faceTipY) + faceMaxX

    bottomY = fullBottomY
    bottomLeftX = max fullLeftX faceBottomLeftX
    bottomRightX = min fullRightX faceBottomRightX
    topY = min fullTopY faceTipY
    topLeftX = max fullLeftX (faceTipX * topY / faceTipY)
    topRightX = min fullRightX (((faceTipX - faceMaxX) * topY / faceTipY) + faceMaxX)

    -- TODO: Determine if this is the right notion of "center" for my uses.
    centerX = (bottomLeftX + bottomRightX + topLeftX + topRightX) / 4
    centerY = (bottomY + topY) / 2

    nonEmptyBucketFaceIntersection =
      fullLeftX <= faceBottomRightX && fullRightX >= faceBottomLeftX


--allBuckets :: DiscreteSurface -> [SurfaceBucketCoordinate]
--allBuckets surface = []

-- TODO: Maybe making an instance of Foldable or something will make it so
-- that I don't have to implement all of these functions over and over
-- again.

lookupFaceField :: SurfaceField v a -> FaceIndex -> FaceField v a
lookupFaceField (SurfaceField faceFields) (FaceIndex faceIndex) =
  faceFields ! faceIndex

readBucketFromFaceField :: (GV.Vector v a, Num a) => FaceField v a -> FaceBucketIndex -> a
readBucketFromFaceField (FaceField values _ _ _) (FaceBucketIndex faceBucketIndex) =
  maybe 0 id $ values GV.!? faceBucketIndex

readBucketFromSurfaceField :: (GV.Vector v a, Num a) => SurfaceField v a -> SurfaceBucketCoordinate -> a
readBucketFromSurfaceField surfaceField (SurfaceBucketCoordinate faceIndex faceBucketIndex) =
  readBucketFromFaceField (lookupFaceField surfaceField faceIndex) faceBucketIndex

mapFaceField :: (GV.Vector v a, GV.Vector v b) => (a -> b) -> FaceField v a -> FaceField v b
mapFaceField f faceField =
  faceField { values = GV.map f (values faceField) }

mapSurfaceField :: (GV.Vector v a, GV.Vector v b) => (a -> b) -> SurfaceField v a -> SurfaceField v b
mapSurfaceField f (SurfaceField faceFields) = SurfaceField $
  GV.map (\faceField -> mapFaceField f faceField) faceFields

zipFaceFieldWith :: (GV.Vector v a, GV.Vector v b, GV.Vector v c) => (a -> b -> c) -> FaceField v a -> FaceField v b -> FaceField v c
zipFaceFieldWith f faceField1 faceField2 =
  faceField1 { values = GV.zipWith f (values faceField1) (values faceField2) }

zipSurfaceFieldWith :: (GV.Vector v a, GV.Vector v b, GV.Vector v c) => (a -> b -> c) -> SurfaceField v a -> SurfaceField v b -> SurfaceField v c
zipSurfaceFieldWith f (SurfaceField faceFields1) (SurfaceField faceFields2) = SurfaceField $
  GV.zipWith (\faceField1 faceField2 -> zipFaceFieldWith f faceField1 faceField2) faceFields1 faceFields2


