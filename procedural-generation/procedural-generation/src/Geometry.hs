module Geometry where

import Linear.V2 (V2)

{-
- A covering from a covering space (embedded in euclidean space) to
- a target space (sort of embedded in euclidean space except for the fact
- that the edges don't necessarily join up).
-
- `forwards` takes a vector in the euclidean space that the covering space
- is embedded in, and returns a vector in the euclidean space that the
- target space is embedded in
-
- `backwards` takes a vector in the euclidean space that the target space
- is embedded in, and returns a vector in the euclidean space that the
- covering space is embedded in
-
- The target space is bounded between (0, 0) and `targetSpaceBound` in the
- euclidean space that it is embedded in.
-}
data Covering = Covering
    { forwards :: V2 Double -> TargetSpaceVector
    , backwards :: V2 Double -> CoveringSpaceVector
    , targetSpaceBound :: V2 Double }

data TargetSpaceVector = InTargetSpace (V2 Double) | OutOfTargetSpace EdgeType

data CoveringSpaceVector = InCoveringSpace (V2 Double) | OutOfCoveringSpace

data EdgeType = Ledge | Wall

testIn :: V2 Double -> V2 Double -> V2 Double -> Bool
testIn lower upper testee = True

bounded :: V2 Double -> EdgeType -> Covering
bounded targetSpaceBound edgeType = Covering
    { forwards = \coveringSpaceVector ->
        if testIn (V2 0 0) targetSpaceBound coveringSpaceVector
            then InTargetSpace coveringSpaceVector
            else OutOfTargetSpace edgeType
    , backwards = \targetSpaceVector ->
        if testIn (V2 0 0) targetSpaceBound targetSpaceVector
            then InCoveringSpace targetSpaceVector
            else OutOfCoveringSpace
    , targetSpaceBound = targetSpaceBound
    }

modulo :: V2 Double -> V2 Double -> V2 Double
modulo modulus x = V2 0 0

sphere :: V2 Double -> Covering
sphere targetSpaceBound = Covering
    { forwards = \coveringSpaceVector -> modulo targetSpaceBound coveringSpaceVector
    , backwards = \targetSpaceVector ->
        if testIn (V2 0 0) targetSpaceBound targetSpaceVector
            then InCoveringSpace targetSpaceVector
            else OutOfCoveringSpace
    , targetSpaceBound = targetSpaceBound
    }

data Quantization = Quantization { spaceSize :: V2 Double, quantizedSize :: V2 Int }

quantize :: Quantiztion -> V2 Double -> [Double, V2 Int]
quantize quantization point = []

continuize :: Quanitization -> V2 Int -> V2 Double
continuize quantization point = V2 0 0
