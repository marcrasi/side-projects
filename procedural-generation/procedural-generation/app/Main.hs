module Main where

import Control.Monad
import Control.Monad.ST (runST)

import qualified Data.Vector as Vector

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
  thawedFaceFields <- mapM thawFaceField $ toList faceFields
  return $ Primitives.SurfaceField $ fromList thawedFaceFields

freezeFaceField faceField = do
  frozenValues <- Vector.freeze $ Primitives.values faceField
  return $ faceField { Primitives.values = frozenValues }

freezeSurfaceField (Primitives.SurfaceField faceFields) = do
  frozenFaceFields <- mapM freezeFaceField $ toList faceFields
  return $ Primitives.SurfaceField $ fromList frozenFaceFields

-- TODO: Discrete surface coordinate types.
writeSurfaceField (Primitives.SurfaceField surfaceField) (Primitives.SurfaceCoordinate ..)

drawGeodesic :: DiscreteSurface -> Primitives.SurfaceField Vector (Color4 GLubyte) -> Primitives.SurfaceField Vector (Color4 GLubyte)
drawGeodesic surface canvas = runST $ do
  mutableCanvas <- thawSurfaceField canvas
  mapM_ (drawPoint mutableCanvas) [0,0.01..2]
  return $ freezeSurfaceField mutableCanvas
  where
    drawPoint mutableCanvas t =

main :: IO ()
main = do
  Display.doInitialize
  let cube = DiscreteSurfaces.cube
  let surfaceField = Primitives.createSurfaceField 0.1 (Color4 0 255 255 255) cube
  preparedTextures <- DisplayGeometry.prepareTextures cube surfaceField
  Display.doDisplay $ DisplayGeometry.displayDiscreteSurface cube preparedTextures
