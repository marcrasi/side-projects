module Main where

import Control.Monad

import qualified Geometry2.DiscreteSurfaces as DiscreteSurfaces
import qualified Display as Display 
import qualified Display.Geometry as DisplayGeometry

main :: IO ()
main = do
  textureName <- DisplayGeometry.prepareTexture DisplayGeometry.checkers
  Display.doDisplay $ DisplayGeometry.displayDiscreteSurface DiscreteSurfaces.cube textureName
