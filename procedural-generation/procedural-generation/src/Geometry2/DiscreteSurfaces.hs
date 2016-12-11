module Geometry2.DiscreteSurfaces where

import Data.Vector (fromList)
import Linear.V3 (V3(V3))

import Geometry2.Primitives
  ( VertexIndex(VertexIndex)
  , Vertex(Vertex)
  , Face(Face)
  , DiscreteSurface
  , makeDiscreteSurface
  )

cube :: DiscreteSurface
cube = makeDiscreteSurface vertices faces
  where
    vertices = fromList
      [ Vertex $ V3 0 0 0
      , Vertex $ V3 1 0 0
      , Vertex $ V3 1 1 0
      , Vertex $ V3 0 1 0
      , Vertex $ V3 0 0 1
      , Vertex $ V3 1 0 1
      , Vertex $ V3 1 1 1
      , Vertex $ V3 0 1 1
      ]
    faces = fromList (
      (mkFaces 0 1 2 3) ++
      (mkFaces 1 5 6 2) ++
      (mkFaces 4 5 6 7) ++
      (mkFaces 0 3 7 4) ++
      (mkFaces 2 6 7 3) ++
      (mkFaces 0 1 5 4))

square :: DiscreteSurface
square = makeDiscreteSurface vertices faces
  where
    vertices = fromList
      [ Vertex $ V3 0 0 0
      , Vertex $ V3 1 0 0
      , Vertex $ V3 1 1 0
      , Vertex $ V3 0 1 0
      ]
    faces = fromList $ mkFaces 0 1 2 3

mkFaces a b c d = [Face (VertexIndex a) (VertexIndex b) (VertexIndex c), Face (VertexIndex a) (VertexIndex c) (VertexIndex d)]
