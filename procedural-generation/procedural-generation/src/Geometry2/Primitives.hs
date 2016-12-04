module Geometry2.Primitives where

import qualified Data.Map.Strict as Map
import Data.Vector (Vector, fromList, toList)
import Linear
  ( V2(V2)
  , V3(V3)
  , cross
  , distance
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

data FaceIndex = FaceIndex Int
  deriving (Eq, Ord, Show)

data EdgeIndex = EdgeIndex Int
  deriving (Eq, Ord, Show)

data Face = Face VertexIndex VertexIndex VertexIndex
  deriving Show

data EmbeddedFace = EmbeddedFace Vertex Vertex Vertex
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
    , vertexToEdges :: Map.Map VertexIndex [EdgeIndex]
    , vertexToFaces :: Map.Map VertexIndex [FaceIndex]
    }
  deriving Show

makeDiscreteSurface :: Vector Vertex -> Vector Face -> DiscreteSurface
makeDiscreteSurface vertices faces = DiscreteSurface
  { vertices = vertices
  , edges = edges
  , faces = faces
  , vertexToEdges = vertexToEdges
  , vertexToFaces = vertexToFaces
  }
  where
    edges = fromList $ concatMap (\(Face a b c) -> [Edge a b, Edge b c, Edge c a]) $ toList faces
    indexedEdges = zipWith (\index edge -> (EdgeIndex index, edge)) [0..] $ toList edges
    indexedFaces = zipWith (\index face -> (FaceIndex index, face)) [0..] $ toList faces
    vertexToEdges = Map.fromListWith (++) $ concatMap (\(index, (Edge a b)) -> [(a, [index]), (b, [index])]) indexedEdges
    vertexToFaces = Map.fromListWith (++) $ concatMap (\(index, (Face a b c)) -> [(a, [index]), (b, [index]), (c, [index])]) indexedFaces

{-
 -
 -              * (x2, y2)
 -             /x
 -            /  x
 -           /    x
 -          /      x
 -         /        x
 - (0, 0) *---------* (x1, 0)
 -}
data FaceCoordinate = FaceCoordinate Vec2
  deriving Show

{-
 - `canonicalize origin posXAxis posYPlane` is the unique rigid affine
 - transformation from the triangle to R2 that brings `origin` to the origin,
 - `posXAxis` to a point on the positive x axis, and `posYPlane` to a point
 - in the positive y half plane.
 -}
canonicalize :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec2
canonicalize origin posXAxis posYPlane v = result
  where
    basis1 = posXAxis - origin
    basis2 = posYPlane - origin
    basisN = cross basis1 basis2
    V3 toBasis1Component toBasis2Component _ = inv33 $ transpose $ V3 basis1 basis2 basisN
    toBasisComponents = V2 toBasis1Component toBasis2Component

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

    V2 basis1Component basis2Component = toBasisComponents !* (v - origin)
    result = basis1Component *^ (V2 x1 0) + basis2Component *^ (V2 x2 y2)

{-
 - Like `canonicalize` on the vertices of the EmbeddedFace, with the
 - additional guarantee that the `posYPlane`'s x-coordinate ends up between
 - 0 and the `posXAxis`'s x-coordinate.
 -}
canonicalizeFace :: EmbeddedFace -> Vec3 -> Vec2
canonicalizeFace (EmbeddedFace (Vertex a) (Vertex b) (Vertex c)) v = result
  where
    attempts =
      [ (a, b, c)
      , (a, c, b)
      , (b, a, c)
      , (b, c, a)
      , (c, a, b)
      , (c, b, a)
      ]
    isSuccess = \(origin, posXAxis, posYPlane) ->
      let
        V2 x1 y1 = canonicalize origin posXAxis posYPlane posXAxis
        V2 x2 y2 = canonicalize origin posXAxis posYPlane posYPlane
      in 0 <= x2 && x2 <= x1
    (successfulOrigin, successfulPosXAxis, successfulPosYPlane) = head $ dropWhile (not . isSuccess) attempts
    result = canonicalize successfulOrigin successfulPosXAxis successfulPosYPlane v

--ambientToFace :: Face -> Vec3 -> FaceCoordinate

--faceToAmbient :: Face -> FaceCoordinate -> Vec3

{-
 - An index into the vector of faces, plus a coordinate on the face.
 -}
data SurfaceCoordinate = SurfaceCoordinate
  { face :: Int
  , faceCoordinate :: FaceCoordinate
  }
  deriving Show

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
