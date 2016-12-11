module Geometry2.Primitives where

import qualified Data.Map.Strict as Map
import Data.Vector (Vector, fromList, toList, (!))
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

data FaceIndex = FaceIndex Int
  deriving (Eq, Ord, Show)

data EdgeIndex = EdgeIndex Int
  deriving (Eq, Ord, Show)

data Face = Face VertexIndex VertexIndex VertexIndex
  deriving Show

data EmbeddedFace = EmbeddedFace IndexedVertex IndexedVertex IndexedVertex
  deriving Show

data Edge = Edge VertexIndex VertexIndex
  deriving Show

{-
 - A polyhedral discrete surface.
 -}
data DiscreteSurface = DiscreteSurface
    { vertices :: Vector Vertex
    , edges :: Vector Edge
    , faces :: Vector Face
    , embeddedFaces :: Vector EmbeddedFace
    , coordinateTransforms :: Vector CoordinateTransform
    , vertexToEdges :: Map.Map VertexIndex [EdgeIndex]
    , vertexToFaces :: Map.Map VertexIndex [FaceIndex]
    }
  deriving Show

makeDiscreteSurface :: Vector Vertex -> Vector Face -> DiscreteSurface
makeDiscreteSurface vertices faces = DiscreteSurface
  { vertices = vertices
  , edges = edges
  , faces = faces
  , embeddedFaces = embeddedFaces
  , coordinateTransforms = coordinateTransforms
  , vertexToEdges = vertexToEdges
  , vertexToFaces = vertexToFaces
  }
  where
    edges = fromList $ concatMap (\(Face a b c) -> [Edge a b, Edge b c, Edge c a]) $ toList faces
    embeddedFaces = fromList $ map (makeEmbeddedFace vertices) $ toList faces
    coordinateTransforms = fromList $ map canonicalCoordinateTransform $ toList embeddedFaces
    indexedEdges = zipWith (\index edge -> (EdgeIndex index, edge)) [0..] $ toList edges
    indexedFaces = zipWith (\index face -> (FaceIndex index, face)) [0..] $ toList faces
    vertexToEdges = Map.fromListWith (++) $ concatMap (\(index, (Edge a b)) -> [(a, [index]), (b, [index])]) indexedEdges
    vertexToFaces = Map.fromListWith (++) $ concatMap (\(index, (Face a b c)) -> [(a, [index]), (b, [index]), (c, [index])]) indexedFaces

lookupVertex :: Vector Vertex -> VertexIndex -> Vertex
lookupVertex vertices (VertexIndex i) = vertices ! i

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

-- Returns `t > 0` such that `start + t * direction` hits the line.
-- Returns +Inf if `start + t * direction (t > 0)` never hits the line.
-- TODO: I should also make this only "detect" intersections where you're
-- leaving the enclosure, so that if I start slightly outside then I'll
-- still get reasonable results.
{-distanceToLine :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Double
distanceToLine a b start direction = 0

-- To transform the position, simply go into ambient coordinates and then
-- back into face coordinates.
-- To transform the direction, calculate the angle between it and the edge
-- that we're leaving. Then make a new direction with the same angle away
-- from the edge that we're arriving at.
jumpAcrossEdge :: DiscreteSurface -> SurfaceCoordinate -> Vec2 -> (SurfaceCoordinate, Vec2)
jumpAcrossEdge surface position direction = 0

advanceGeodesic :: DiscreteSurface -> SurfaceCoordinate -> Vec2 -> Double -> SurfaceCoordinate
advanceGeodesic surface (SurfaceCoordinate (FaceIndex startFaceIndex) start) direction t =
  if closestEdgeDistance >= t
    then SurfaceCoordinate (FaceIndex startFaceIndex) (start + t *^ direction)
    else advanceGeodesic surface newFaceStart newFaceDirection (t - closestEdgeDistance)
  where
    face = (faces surface) ! startFaceIndex
    coordinateTransform = (coordinateTransforms surface) ! startFaceIndex
    origin = V2 0 0

    -- Calculate how much distance it is to each of the boundary edges.
    e1Distance = distanceToLine origin (faceVertex1 coordinateTransform) start direction
    e2Distance = distanceToLine origin (faceVertex2 coordinateTransform) start direction
    e3Distance = distanceToLine (faceVertex1 coordinateTransform) (faceVertex2 coordinateTransform) start direction

    -- Find the closest boundary edge.
    (closestEdgeDistance, closestEdge) = mininumBy (\(d1, _) (d2, _) -> d1 < d2) [(e1Distance, 1), (e2Distance, 2), (e3Distance, 3)]

    -- Advance to that edge and jump across it.
    atBoundary = SurfaceCoordinate (FaceIndex startFaceIndex) (start + closestEdgeDistance *^ direction)
    (newFaceStart, newFaceDirection) = jumpAcrossEdge surface atBoundary direction-}

{-
 - A discrete field of quantities of type `a` on a face.
 -
 - The fineness is the side length of the (square) bucket, in ambient
 - space.
 -
 - TODO: Make `values` be an unboxed Vector. Also make it possible to
 - decide whether `values` should be mutable.
 -}
data FaceField a = FaceField
  { values :: Vector a
  , fineness :: Double
  }
  deriving Show

{-
 - One FaceField per face.
 -}
data SurfaceField a = SurfaceField (Vector (FaceField a))
  deriving Show
