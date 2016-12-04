module Display.Geometry where

import Data.Vector (toList, (!))

import Linear (V3(V3), cross, normalize)

import Graphics.UI.GLUT

import Geometry2.Primitives
  ( DiscreteSurface(DiscreteSurface)
  , Vertex(Vertex)
  , Face(Face)
  )

displayDiscreteSurface :: DiscreteSurface -> DisplayCallback
displayDiscreteSurface (DiscreteSurface vertices faces) = do
  mapM_ displayFace faces

  where
    displayFace (Face a b c) = do
      let Vertex va = vertices ! a
      let Vertex vb = vertices ! b
      let Vertex vc = vertices ! c
      renderPrimitive Polygon $ do
        setNormal va vb vc
        mapM_ vertex3f [ va, vb, vc ]
      return ()

    vertex3f (V3 vx vy vz) = vertex $ Vertex3 vx vy vz

    setNormal va vb vc = do
      let (V3 nx ny nz) = Linear.normalize $ cross (vb - va) (vc - va)
      normal $ Normal3 nx ny nz
