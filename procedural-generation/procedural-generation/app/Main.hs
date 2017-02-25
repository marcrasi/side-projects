module Main where

import Control.Monad
import Control.Monad.ST (runST)

import Debug.Trace

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Linear (V2(V2))

import qualified Geometry2.DiscreteSurfaces as DiscreteSurfaces
import qualified Geometry2.Geodesic as Geodesic
import qualified Geometry2.Primitives as Primitives
import qualified Display as Display
import qualified Display.Geometry as DisplayGeometry
import qualified Wave as Wave

import Graphics.UI.GLUT

thawFaceField faceField = do
  thawedValues <- Vector.thaw $ Primitives.values faceField
  return $ faceField { Primitives.values = thawedValues }

thawSurfaceField (Primitives.SurfaceField faceFields) = do
  thawedFaceFields <- mapM thawFaceField $ Vector.toList faceFields
  return $ Primitives.SurfaceField $ Vector.fromList thawedFaceFields

freezeFaceField faceField = do
  frozenValues <- Vector.freeze $ Primitives.values faceField
  return $ faceField { Primitives.values = frozenValues }

freezeSurfaceField (Primitives.SurfaceField faceFields) = do
  frozenFaceFields <- mapM freezeFaceField $ Vector.toList faceFields
  return $ Primitives.SurfaceField $ Vector.fromList frozenFaceFields

writeFaceField faceField (Primitives.FaceBucketIndex faceBucketIndex) value =
  --if faceBucketIndex >= 0 && faceBucketIndex < (Primitives.xBucketCount faceField) * (Primitives.yBucketCount faceField)
  --  then MVector.write (Primitives.values faceField) faceBucketIndex value
  --  else return ()
  MVector.write (Primitives.values faceField) faceBucketIndex value

-- TODO: Discrete surface coordinate types.
writeSurfaceField (Primitives.SurfaceField faceFields) (Primitives.SurfaceBucketCoordinate (Primitives.FaceIndex faceIndex) faceBucketIndex) value =
  writeFaceField (faceFields Vector.! faceIndex) faceBucketIndex value

scalarFieldToGrayscale :: Primitives.SurfaceField Vector.Vector Double -> Primitives.SurfaceField Vector.Vector (Color4 GLubyte)
scalarFieldToGrayscale surfaceField =
  Primitives.mapSurfaceField (\scalar -> let byte = ((floor $ 256 * scalar) `min` 255) `max` 0 in Color4 byte byte byte 255) surfaceField

drawDisc :: Double -> Primitives.DiscreteSurface -> Primitives.SurfaceField Vector.Vector Double -> Primitives.SurfaceField Vector.Vector Double
drawDisc radius surface canvas = runST $ do
  mutableCanvas <- thawSurfaceField canvas
  mapM_ (drawCircle mutableCanvas) [0,0.01..radius]
  freezeSurfaceField mutableCanvas
  where
    center = Primitives.SurfaceCoordinate (Primitives.FaceIndex 0) (V2 0.3 0.2)
    drawCircle mutableCanvas radius = mapM_ (drawPoint mutableCanvas radius) [0,(0.01/radius)..6.3]
    drawPoint mutableCanvas radius angle =
      let
        point = Geodesic.advanceGeodesic surface center (V2 (cos angle) (sin angle)) radius
      in
        writeSurfaceField mutableCanvas (Primitives.toSurfaceBucketCoordinate mutableCanvas point) 0.1

drawGeodesic :: Primitives.DiscreteSurface -> Primitives.SurfaceField Vector.Vector (Color4 GLubyte) -> Primitives.SurfaceField Vector.Vector (Color4 GLubyte)
drawGeodesic surface canvas = runST $ do
  mutableCanvas <- thawSurfaceField canvas
  mapM_ (drawPoint mutableCanvas) [0,0.01..length]
  freezeSurfaceField mutableCanvas
  where
    length = 50
    start = Primitives.SurfaceCoordinate
      (Primitives.FaceIndex 0)
      (V2 0.3 0.2)
    direction = V2 0.5 0.12
    drawPoint mutableCanvas t =
      let
        foo = Geodesic.advanceGeodesic surface start direction t
      in
        traceShow foo $ writeSurfaceField
          mutableCanvas
          (Primitives.toSurfaceBucketCoordinate
            mutableCanvas
            (foo))
          (Color4 (floor $ 255 * t / length) 0 255 100)

myDisplayCallback :: DisplayGeometry.PreparedTextures -> Wave.WaveState -> IO ()
myDisplayCallback preparedTextures (Wave.WaveState field _) = do
  --let grayscale = scalarFieldToGrayscale field
  let grayscale = scalarFieldToGrayscale $ Wave.laplacian DiscreteSurfaces.cube field
  DisplayGeometry.writeTextures preparedTextures DiscreteSurfaces.cube grayscale
  DisplayGeometry.displayDiscreteSurface DiscreteSurfaces.cube preparedTextures

timeStep :: Double -> Wave.WaveState -> Wave.WaveState
--timeStep dt field = Wave.advance dt DiscreteSurfaces.cube field
timeStep dt field = field

main :: IO ()
main = do
  Display.doInitialize
  let valueField = drawDisc 0.1 DiscreteSurfaces.cube $ Primitives.createSurfaceField 0.02 0 0 DiscreteSurfaces.cube
  let velocityField = Primitives.createSurfaceField 0.02 0 0 DiscreteSurfaces.cube
  let waveState = Wave.WaveState valueField velocityField
  preparedTextures <- DisplayGeometry.prepareTextures DiscreteSurfaces.cube valueField
  Display.doDisplay waveState timeStep (myDisplayCallback preparedTextures)
