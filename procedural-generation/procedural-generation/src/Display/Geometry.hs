module Display.Geometry where

import Data.Vector (fromList, toList, (!))

import Foreign (withArray)

import Linear (V3(V3), cross, normalize)

import Graphics.UI.GLUT

import Geometry2.Primitives
  ( DiscreteSurface(DiscreteSurface)
  , VertexIndex(VertexIndex)
  , Vertex(Vertex)
  , Face(Face)
  , FaceField(FaceField)
  , values
  , fineness
  )

prepareTexture :: FaceField (Color4 GLubyte) -> IO TextureObject
prepareTexture (FaceField values _) = do
  textureName <- genObjectName
  textureBinding Texture2D $= Just textureName
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  errs0 <- get errors
  print errs0
  withArray (toList values) $ (texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D 64 64) 0) . (PixelData RGBA UnsignedByte)
  errs <- get errors
  print errs
  print textureName
  s <- get $ textureSize2D Texture2D 0
  o <- get $ textureProxyOK Texture2D 0
  print s
  print o
  errs2 <- get errors
  print errs2
  return textureName

checkers :: FaceField (Color4 GLubyte)
checkers = FaceField
  { values = fromList
    [ Color4 c 0 0 255 |
      i <- [ 0 .. 63 ],
      j <- [ 0 .. 63 ],
      let
        c
          | (i `div` 8) `mod` 2 == (j `div` 8) `mod` 2 = 0
          | otherwise = 255 ]
  , fineness = 100
  }

displayDiscreteSurface :: DiscreteSurface -> TextureObject -> DisplayCallback
displayDiscreteSurface (DiscreteSurface vertices faces _ _ _ _) textureName = do
  mapM_ displayFace faces
  where
    displayFace (Face (VertexIndex a) (VertexIndex b) (VertexIndex c)) = do
      let Vertex va = vertices ! a
      let Vertex vb = vertices ! b
      let Vertex vc = vertices ! c
      texture Texture2D $= Enabled
      textureFunction $= Modulate
      textureBinding Texture2D $= Just textureName
      renderPrimitive Polygon $ do
        setNormal va vb vc
        texCoord $ TexCoord2 0 (0 :: GLfloat)
        vertex3f va
        texCoord $ TexCoord2 1 (0 :: GLfloat)
        vertex3f vb
        texCoord $ TexCoord2 0 (1 :: GLfloat)
        vertex3f vc
      texture Texture2D $= Disabled
      flush

    vertex3f (V3 vx vy vz) = vertex $ Vertex3 vx vy vz

    setNormal va vb vc = do
      let (V3 nx ny nz) = Linear.normalize $ cross (vb - va) (vc - va)
      normal $ Normal3 nx ny nz
