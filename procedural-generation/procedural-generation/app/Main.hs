module Main where

import Control.Monad

import qualified Geometry2.DiscreteSurfaces as DiscreteSurfaces
import qualified Display as Display 
import qualified Display.Geometry as DisplayGeometry

main :: IO ()
main = Display.doDisplay $ DisplayGeometry.displayDiscreteSurface DiscreteSurfaces.cube 
