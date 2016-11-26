module Geometry where

import Linear.V2 (V2)

data Range a = InRange a | OutOfRange EdgeType 

data Domain a = InDomain a | OutOfDomain

data Covering = { forwards :: V2 Double -> Range (V2 Double), backwards :: V2 Double -> Domain (V2 Double) }

data EdgeType = Ledge | Wall
