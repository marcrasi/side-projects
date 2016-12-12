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
writeSurfaceField (Primitives.SurfaceField faceFields) (Primitives.SurfaceBucketCoordinates (Primitives.FaceIndex faceIndex) faceBucketIndex) value =
  writeFaceField (faceFields Vector.! faceIndex) faceBucketIndex value

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
          (Primitives.toSurfaceBucketCoordinates
            mutableCanvas
            (foo))
          (Color4 (floor $ 255 * t / length) 0 255 100)

main :: IO ()
main = do
  Display.doInitialize
  let cube = DiscreteSurfaces.cube
  let surfaceField = Primitives.createSurfaceField 0.021 (Color4 100 100 100 100) (Color4 100 100 100 100) cube
  let surfaceField2 = drawGeodesic cube surfaceField
  preparedTextures <- DisplayGeometry.prepareTextures cube surfaceField2
  Display.doDisplay $ DisplayGeometry.displayDiscreteSurface cube preparedTextures
