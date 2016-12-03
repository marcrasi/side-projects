module Main where

import Control.Monad

import qualified Geometry2.DiscreteSurfaces as DiscreteSurfaces

main :: IO ()
main = print $ DiscreteSurfaces.cube
