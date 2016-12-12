module Geometry2.Primitives where

import qualified Data.Map.Strict as Map
import Data.Vector (Vector, fromList, toList, (!))
import qualified Data.Vector.Generic as GV
import Linear
  ( V2(V2)
  , V3(V3)
  , cross
  , distance
  , inv22
  , inv33
  , transpose
  , (!*)
  , (!*!)
  , (*^)
  )

type Vec2 = V2 Double
type Vec3 = V3 Double

{-
 - Embedding into R^3.
 -}
data Vertex = Vertex Vec3
  deriving Show

data VertexIndex = VertexIndex Int
  deriving (Eq, Ord, Show)

data IndexedVertex = IndexedVertex VertexIndex Vertex
  deriving (Show)

getVertexIndex :: IndexedVertex -> VertexIndex
getVertexIndex (IndexedVertex i _) = i

data FaceIndex = FaceIndex Int
  deriving (Eq, Ord, Show)

data Edge = Edge VertexIndex VertexIndex
  deriving (Eq, Ord, Show)

makeEdge :: IndexedVertex -> IndexedVertex -> Edge
makeEdge (IndexedVertex i1 _) (IndexedVertex i2 _) = Edge i1 i2

data Face = Face VertexIndex VertexIndex VertexIndex
  deriving Show

data EmbeddedFace = EmbeddedFace IndexedVertex IndexedVertex IndexedVertex
  deriving Show

{-
 - A polyhedral discrete surface.
 -}
data DiscreteSurface = DiscreteSurface
    { vertices :: Vector Vertex
    , faces :: Vector Face
    , embeddedFaces :: Vector EmbeddedFace
    , coordinateTransforms :: Vector CoordinateTransform
    , vertexToFaces :: Map.Map VertexIndex [FaceIndex]
    , edgeToFaces :: Map.Map Edge [FaceIndex]
    }
  deriving Show

makeDiscreteSurface :: Vector Vertex -> Vector Face -> DiscreteSurface
makeDiscreteSurface vertices faces = DiscreteSurface
  { vertices = vertices
  , faces = faces
  , embeddedFaces = embeddedFaces
  , coordinateTransforms = coordinateTransforms
  , vertexToFaces = vertexToFaces
  , edgeToFaces = edgeToFaces
  }
  where
    embeddedFaces = fromList $ map (makeEmbeddedFace vertices) $ toList faces
    coordinateTransforms = fromList $ map canonicalCoordinateTransform $ toList embeddedFaces
    indexedFaces = zipWith (\index face -> (FaceIndex index, face)) [0..] $ toList faces
    vertexToFaces = Map.fromListWith (++) $ concatMap (\(index, (Face a b c)) -> [(a, [index]), (b, [index]), (c, [index])]) indexedFaces
    edgeToFaces = Map.fromListWith (++) $ concatMap (\(index, (Face a b c)) -> [(Edge a b, [index]), (Edge b a, [index]), (Edge b c, [index]), (Edge c b, [index]), (Edge a c, [index]), (Edge c a, [index])]) indexedFaces

lookupVertex :: Vector Vertex -> VertexIndex -> Vertex
lookupVertex vertices (VertexIndex i) = vertices ! i

lookupFace :: Vector Face -> FaceIndex -> Face
lookupFace faces (FaceIndex i) = faces ! i

lookupCoordinateTransform :: Vector CoordinateTransform -> FaceIndex -> CoordinateTransform
lookupCoordinateTransform coordinateTransforms (FaceIndex i) = coordinateTransforms ! i

makeEmbeddedFace :: Vector Vertex -> Face -> EmbeddedFace
makeEmbeddedFace vertices (Face a b c) =
  EmbeddedFace
    (IndexedVertex a (lookupVertex vertices a))
    (IndexedVertex b (lookupVertex vertices b))
    (IndexedVertex c (lookupVertex vertices c))

data CoordinateTransform = CoordinateTransform
  { toFace :: Vec3 -> Vec2
  , toAmbient :: Vec2 -> Vec3
  , vertex0 :: IndexedVertex -- the origin
  , vertex1 :: IndexedVertex -- the positive x axis vertex
  , vertex2 :: IndexedVertex -- the positive y plane vertex
  , faceVertex0 :: Vec2 -- face coordinates for vertex 0
  , faceVertex1 :: Vec2 -- face coordinates for vertex 1
  , faceVertex2 :: Vec2 -- face coordinates for vertex 2
  }

instance (Show CoordinateTransform) where
  show _ = "CoordinateTransform"

{-
 - `makeCoordinateTransform origin posXAxis posYPlane` is the unique rigid affine
 - transformation from the triangle to R2 that brings `origin` to the origin,
 - `posXAxis` to a point on the positive x axis, and `posYPlane` to a point
 - in the positive y half plane.
 -}
makeCoordinateTransform :: IndexedVertex -> IndexedVertex -> IndexedVertex -> CoordinateTransform
makeCoordinateTransform originIV posXAxisIV posYPlaneIV =
  CoordinateTransform
    { toFace = toFace
    , toAmbient = toAmbient
    , vertex0 = originIV
    , vertex1 = posXAxisIV
    , vertex2 = posYPlaneIV
    , faceVertex0 = V2 0 0
    , faceVertex1 = faceBasis1
    , faceVertex2 = faceBasis2
    }
  where
    (IndexedVertex _ (Vertex origin)) = originIV
    (IndexedVertex _ (Vertex posXAxis)) = posXAxisIV
    (IndexedVertex _ (Vertex posYPlane)) = posYPlaneIV

    ambientBasis1 = posXAxis - origin
    ambientBasis2 = posYPlane - origin
    ambientBasisN = cross ambientBasis1 ambientBasis2
    ambientToBasisCoordinates = inv33 $ transpose $ V3 ambientBasis1 ambientBasis2 ambientBasisN

    x1 = distance origin posXAxis

    l2 = distance posXAxis posYPlane
    l3 = distance posYPlane origin

    -- x2^2 + y2^2 = l3^2
    -- (x1 - x2)^2 + y2^2 = l2^2
    -- => x2^2 - (x1 - x2)^2 = l3^2 - l2^2
    -- => x2^2 - x1^2 + 2x1x2 - x2^2 = l3^2 - l2^2
    -- => 2x1x2 - x1^2 = l3^2 - l2^2
    -- => x2 = (l3^2 - l2^2 + x1^2) / 2x1
    -- => y2 = sqrt(l3^2 - x2^2)

    x2 = (l3 * l3 - l2 * l2 + x1 * x1) / (2 * x1)
    y2 = sqrt(l3 * l3 - x2 * x2)

    faceBasis1 = V2 x1 0
    faceBasis2 = V2 x2 y2
    faceToBasisCoordinates = inv22 $ transpose $ V2 faceBasis1 faceBasis2

    toFace v =
      let
        V3 basisCoordinate1 basisCoordinate2 _ = ambientToBasisCoordinates !* (v - origin)
      in
        basisCoordinate1 *^ faceBasis1 + basisCoordinate2 *^ faceBasis2

    toAmbient v =
      let
        V2 basisCoordinate1 basisCoordinate2 = faceToBasisCoordinates !* v
      in
        basisCoordinate1 *^ ambientBasis1 + basisCoordinate2 *^ ambientBasis2 + origin

{-
 -
 -              * (x2, y2) (faceVertex2)
 -             /x
 -            /  x
 -           /    x
 -          /      x
 -         /        x
 - (0, 0) *---------* (x1, 0) (faceVertex1)
 -
 - Guarnatees that 0 <= x2 <= x1
 -}
canonicalCoordinateTransform :: EmbeddedFace -> CoordinateTransform
canonicalCoordinateTransform (EmbeddedFace aIV bIV cIV)
  | tipX < 0 = makeCoordinateTransform bIV cIV aIV
  | tipX >= 0 && tipX <= maxX = transformAttempt
  | otherwise = makeCoordinateTransform aIV cIV bIV
  where
    (IndexedVertex _ (Vertex b)) = bIV
    (IndexedVertex _ (Vertex c)) = cIV
    transformAttempt = makeCoordinateTransform aIV bIV cIV
    (V2 tipX _) = toFace transformAttempt c
    (V2 maxX _) = toFace transformAttempt b

{-
 - An index into the vector of faces, plus a coordinate on the face.
 -}
data SurfaceCoordinate = SurfaceCoordinate
  { face :: FaceIndex
  , faceCoordinate :: Vec2
  }
  deriving Show

surfaceToAmbient :: DiscreteSurface -> SurfaceCoordinate -> Vec3
surfaceToAmbient surface (SurfaceCoordinate face faceCoordinate) =
  toAmbient coordinateTransform faceCoordinate
  where
    coordinateTransform = lookupCoordinateTransform (coordinateTransforms surface) face

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
 - Buckets fully outside the face are meaningless.
 -
 - Buckets partially outside the face shall be interpreted as buckets
 - containing the intersection between the face and the "normal" bucket
 - square.
 -
 - TODO: Make `values` be an unboxed Vector.
 -}
data FaceField v a = FaceField
  { values :: v a
  , fineness :: Double
  , xBucketCount :: Int
  , yBucketCount :: Int
  }

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

data FaceBucketIndex = FaceBucketIndex Int

toFaceBucketIndex :: FaceField v a -> Vec2 -> FaceBucketIndex
toFaceBucketIndex (FaceField _ fineness xBucketCount _) (V2 x y) =
  FaceBucketIndex $ (floor $ x / fineness) + xBucketCount * (floor $ y / fineness)

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

data SurfaceBucketCoordinates = SurfaceBucketCoordinates
  { bFace :: FaceIndex
  , faceBucketIndex :: FaceBucketIndex
  }

toSurfaceBucketCoordinates :: SurfaceField v a -> SurfaceCoordinate -> SurfaceBucketCoordinates
toSurfaceBucketCoordinates (SurfaceField faceFields) (SurfaceCoordinate (FaceIndex faceIndex) faceCoordinate) =
  SurfaceBucketCoordinates
    (FaceIndex faceIndex)
    (toFaceBucketIndex (faceFields ! faceIndex) faceCoordinate)
