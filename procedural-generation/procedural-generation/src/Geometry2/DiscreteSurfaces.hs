module Geometry2.DiscreteSurfaces where

import Data.Vector (fromList)
import Linear.V3 (V3)

import Primitives (Vertex, Face, DiscreteSurface)

cube :: DiscreteSurface
cube = DiscreteSurface
    { vertices = fromList
        [ Vertex $ V3 0 0 0
        , Vertex $ V3 1 0 0
        , Vertex $ V3 1 1 0
        , Vertex $ V3 0 1 0
        , Vertex $ V3 0 0 1
        , Vertex $ V3 1 0 1
        , Vertex $ V3 1 1 1
        , Vertex $ V3 0 1 1
        ]
    , faces = fromList (
        mkFaces 0 1 2 3 ++
        mkFaces 2 5 6 3 ++
        mkFaces 4 5 6 7 ++
        mkFaces 0 3 7 4 ++
        mkFaces 2 3 6 7 ++
        mkFaces 0 1 5 4)
    }
where
    mkFaces a b c d = [Face a b c, Face a c d]
