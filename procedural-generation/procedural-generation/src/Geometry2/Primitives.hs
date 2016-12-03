module Geometry2.Primitives where

import Data.Vector (Vector)
import Linear.V2 (V2)
import Linear.V3 (V3)

type Vec2 = V2 Double
type Vec3 = V3 Double

{-
 - Embedding into R^3.
 -}
data Vertex = Vertex Vec3

{-
 - Three indices into the vector of vertices.
 -}
data Face = Face Int Int Int

{-
 - A polyhedral discrete surface.
 -}
data DiscreteSurface = DiscreteSurface
    { vertices :: Vector Vertex
    , faces :: Vector Face
    }

{-
 - (0, 1) *
 -        | x
 -        |   x
 -        |     x
 -        |       x
 - (0, 0) *---------* (1, 0)
 -}
data FaceCoordinate = FaceCoordinate Vec2

{-
 - An index into the vector of faces, plus a coordinate on the face.
 -}
data SurfaceCoordinate = SurfaceCoordinate
    { face :: Int
    , faceCoordinate :: FaceCoordinate
    }

{-
 - A discrete field of quantities of type `a` on a face.
 -
 - The fineness is the number of buckets between 0 and 1 in
 - face-coordinates.
 -
 - TODO: Make `values` be an unboxed Vector. Also make it possible to
 - decide whether `values` should be mutable.
 -}
data FaceField a = FaceField
    { values :: Vector a
    , fineness :: Int
    }

{-
 - One FaceField per face.
 -}
data SurfaceField a = SurfaceField (Vector (FaceField a))

