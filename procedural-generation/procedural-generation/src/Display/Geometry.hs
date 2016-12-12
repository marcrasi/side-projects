module Display.Geometry where

import Control.Monad (zipWithM, zipWithM_)
import Data.Vector (Vector, fromList, toList, (!))
import GHC.Float (double2Float)
import Foreign (withArray)

import Linear (V2(V2), V3(V3), cross, normalize)

import Graphics.UI.GLUT

import Geometry2.Primitives
  ( DiscreteSurface(DiscreteSurface)
  , VertexIndex(VertexIndex)
  , IndexedVertex(IndexedVertex)
  , Vertex(Vertex)
  , Face(Face)
  , FaceField(FaceField)
  , SurfaceField(SurfaceField)
  , CoordinateTransform
  , coordinateTransforms
  , vertex0
  , vertex1
  , vertex2
  , faceVertex0
  , faceVertex1
  , faceVertex2
  , values
  , fineness
  )

data PreparedTexture = PreparedTexture
  { textureName :: TextureObject
  , vertex0TexCoord :: TexCoord2 GLfloat
  , vertex1TexCoord :: TexCoord2 GLfloat
  , vertex2TexCoord :: TexCoord2 GLfloat
  }

prepareTexture :: CoordinateTransform -> FaceField Vector (Color4 GLubyte) -> IO PreparedTexture
prepareTexture coordinateTransform (FaceField values fineness xBucketCount yBucketCount) = do
  textureName <- genObjectName

  textureBinding Texture2D $= Just textureName
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

  let textureSize = TextureSize2D (fromIntegral xBucketCount) (fromIntegral yBucketCount)
  withArray (toList values) $ (texImage2D Texture2D NoProxy 0 RGBA' textureSize 0) . (PixelData RGBA UnsignedByte)

  return $ PreparedTexture
    { textureName = textureName
    , vertex0TexCoord = makeTexCoord $ faceVertex0 coordinateTransform
    , vertex1TexCoord = makeTexCoord $ faceVertex1 coordinateTransform
    , vertex2TexCoord = makeTexCoord $ faceVertex2 coordinateTransform
    }
  where
    makeTexCoord (V2 x y) = TexCoord2
      (double2Float $ x / (fineness * (fromIntegral xBucketCount)))
      (double2Float $ y / (fineness * (fromIntegral yBucketCount)))

prepareTextures :: DiscreteSurface -> SurfaceField Vector (Color4 GLubyte) -> IO (Vector PreparedTexture)
prepareTextures surface (SurfaceField faceFields) = do
  preparedTextures <- zipWithM prepareTexture (toList $ coordinateTransforms surface) (toList faceFields)
  return $ fromList preparedTextures

displayDiscreteSurface :: DiscreteSurface -> Vector PreparedTexture -> DisplayCallback
displayDiscreteSurface surface preparedTextures = do


  materialAmbient Front $= Color4 1 1 1 1
  materialDiffuse Front $= Color4 1 1 1 1
  materialAmbient Back $= Color4 1 1 1 1
  materialDiffuse Back $= Color4 1 1 1 1

  zipWithM_ displayFace (toList $ coordinateTransforms surface) (toList preparedTextures)

  flush

  where
    displayFace coordinateTransform preparedTexture = do
      let IndexedVertex _ (Vertex v0) = vertex0 coordinateTransform
      let IndexedVertex _ (Vertex v1) = vertex1 coordinateTransform
      let IndexedVertex _ (Vertex v2) = vertex2 coordinateTransform
      texture Texture2D $= Enabled
      textureFunction $= Modulate
      textureBinding Texture2D $= Just (textureName preparedTexture)
      renderPrimitive Polygon $ do
        setNormal v0 v1 v2
        texCoord $ vertex0TexCoord preparedTexture
        vertex3f v0
        texCoord $ vertex1TexCoord preparedTexture
        vertex3f v1
        texCoord $ vertex2TexCoord preparedTexture
        vertex3f v2
      texture Texture2D $= Disabled

    vertex3f (V3 vx vy vz) = vertex $ Vertex3 vx vy vz

    setNormal va vb vc = do
      let (V3 nx ny nz) = Linear.normalize $ cross (vb - va) (vc - va)
      normal $ Normal3 nx ny nz
