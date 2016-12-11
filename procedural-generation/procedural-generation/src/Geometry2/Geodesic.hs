module Geometry2.Geodesic where

import Geometry2.Primitives

import Data.List (minimumBy)
import qualified Data.Map.Strict as Map
import Linear
  ( V2(V2)
  , dot
  , normalize
  , (*^)
  )

perp :: Vec2 -> Vec2
perp (V2 x y) = V2 (-y) x

{-
 - Returns `t > 0` such that `start + t * direction` exits the c-side
 - halfplane of the a-b line.
 - Returns +Inf if `start + t * direction (t > 0)` never exits the
 - halfplane or if `start` is already outside the halfplane.
 -}
distanceToExitHalfplane :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Vec2 -> Double
distanceToExitHalfplane (V2 0 0) b c start direction =
  if startInHalfplane && t > 0
    then t
    else read "Infinity"
  where
    ub = normalize b
    perp_ub = perp ub
    start_perpB = dot start perp_ub
    c_perpB = dot c perp_ub
    startInHalfplane = start_perpB * c_perpB > 0
    direction_perpB = dot direction perp_ub
    t = -start_perpB / direction_perpB
distanceToExitHalfplane a b c start direction =
  distanceToExitHalfplane (V2 0 0) (b - a) (c - a) (start - a) direction

getFaceVertex :: CoordinateTransform -> VertexIndex -> Vec2
getFaceVertex coordinateTransform vertexIndex
  | (getVertexIndex $ vertex0 coordinateTransform) == vertexIndex = faceVertex0 coordinateTransform
  | (getVertexIndex $ vertex1 coordinateTransform) == vertexIndex = faceVertex1 coordinateTransform
  | (getVertexIndex $ vertex2 coordinateTransform) == vertexIndex = faceVertex2 coordinateTransform

-- To transform the position, simply go into ambient coordinates and then
-- back into face coordinates.
-- To transform the direction, calculate the angle between it and the edge
-- that we're leaving. Then make a new direction with the same angle away
-- from the edge that we're arriving at.
jumpAcrossEdge :: DiscreteSurface -> SurfaceCoordinate -> Edge -> Vec2 -> (SurfaceCoordinate, Vec2)
jumpAcrossEdge surface (SurfaceCoordinate sourceFace sourcePosition) sourceEdge direction = (jumpedPosition, jumpedDirection)
  where
    sourceFaceTransform = lookupCoordinateTransform (coordinateTransforms surface) sourceFace

    -- TODO: Handle boundary edges that don't touch other faces.
    destinationFace = head $ dropWhile (== sourceFace) $ (edgeToFaces surface) Map.! sourceEdge
    destinationFaceTransform = lookupCoordinateTransform (coordinateTransforms surface) destinationFace

    jumpedPosition = SurfaceCoordinate
      destinationFace
      ((toFace destinationFaceTransform) $ (toAmbient sourceFaceTransform) sourcePosition)

    Edge edgeVertexA edgeVertexB = sourceEdge
    sourceEdgeVertexA = getFaceVertex sourceFaceTransform edgeVertexA
    sourceEdgeVertexB = getFaceVertex sourceFaceTransform edgeVertexB
    desitnationEdgeVertexA = getFaceVertex destinationFaceTransform edgeVertexA
    desitnationEdgeVertexB = getFaceVertex destinationFaceTransform edgeVertexB

    sourceEdgeDirection = normalize $ sourceEdgeVertexB - sourceEdgeVertexA
    sourceEdgePerpDirection = perp sourceEdgeDirection
    destinationEdgeDirection = normalize $ desitnationEdgeVertexB - desitnationEdgeVertexA
    desitnationEdgePerpDirection = perp destinationEdgeDirection

    jumpedDirection =
      (dot direction sourceEdgeDirection) *^ destinationEdgeDirection -
        (dot direction sourceEdgePerpDirection) *^ desitnationEdgePerpDirection

advanceGeodesic :: DiscreteSurface -> SurfaceCoordinate -> Vec2 -> Double -> SurfaceCoordinate
advanceGeodesic surface (SurfaceCoordinate startFaceIndex start) direction t =
  if closestEdgeDistance >= t
    then SurfaceCoordinate startFaceIndex (start + t *^ direction)
    else advanceGeodesic surface newFaceStart newFaceDirection (t - closestEdgeDistance)
  where
    face = lookupFace (faces surface) startFaceIndex
    coordinateTransform = lookupCoordinateTransform (coordinateTransforms surface) startFaceIndex

    -- Calculate how much distance it is to each of the boundary edges.
    fv0 = faceVertex0 coordinateTransform
    fv1 = faceVertex1 coordinateTransform
    fv2 = faceVertex2 coordinateTransform
    iv0 = vertex0 coordinateTransform
    iv1 = vertex1 coordinateTransform
    iv2 = vertex2 coordinateTransform
    e1Distance = distanceToExitHalfplane fv0 fv1 fv2 start direction
    e2Distance = distanceToExitHalfplane fv0 fv2 fv1 start direction
    e3Distance = distanceToExitHalfplane fv1 fv2 fv0 start direction
    e1 = makeEdge iv0 iv1
    e2 = makeEdge iv0 iv2
    e3 = makeEdge iv1 iv2

    -- Find the closest boundary edge.
    (closestEdgeDistance, closestEdge) = minimumBy (\(d1, _) (d2, _) -> compare d1 d2) [(e1Distance, e1), (e2Distance, e2), (e3Distance, e3)]

    -- Advance to that edge and jump across it.
    atBoundary = SurfaceCoordinate startFaceIndex (start + closestEdgeDistance *^ direction)
    (newFaceStart, newFaceDirection) = jumpAcrossEdge surface atBoundary closestEdge direction
