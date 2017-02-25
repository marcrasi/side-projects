module Geometry2.ConvexPolygons where

import Geometry2.Primitives (Vec2)

{-
 - Finds the intersection of the two line segments a-b and c-d.
 -}
getSegmentIntersection :: ((Vec2, Vec2), (Vec2, Vec2)) -> Maybe Vec2
getSegmentIntersection ((a, b), (c, d)) =
    if det > 1e-5 && t >= 0 && t <= 1 && s >= 0 && s <= 1
      then Just t
      else Nothing
  where
    matrix = V2 (b - a) (c - d)
    det = det22 matrix
    inv = inv22 matrix
    V2 t s = inv !* (c - a)

-- Vertices are ordered in the order they appear on the perimeter,
-- counterclockwise.
data ConvexPolygon = ConvexPolygon [Vec2]

getSegments :: ConvexPolygon -> [(Vec2, Vec2)]
getSegments (ConvexPolygon vertices) = zip vertices (tail $ cycle vertices)

getArea :: ConvexPolygon -> Double
getArea polygon = sum $ map (\((V2 x1 y1), (V2 x2 y2)) -> x1 * y2 - x2 * y1) (getSegments polygon)

getCentroid :: ConvexPolygon -> Vec2
getCentroid polygon =
  (1 / 6 * (getArea polygon)) * sum $ map (\((V2 x1 y1), (V2 x2 y2)) -> ((V2 x1 y1) + (V2 x2 y2)) * (x1 * y2 + x2 * y1)) (getSegments polygon)

getIntersection :: ConvexPolygon -> ConvexPolygon -> Maybe ConvexPolygon
getIntersection polygon1 polygon2 =
  makeConvexPolygonFromVertices resultVertices
  where
    ConvexPolygon vertices1 = polygon1
    ConvexPolygon vertices2 = polygon2

    segmentProduct = [(s1, s2) | s1 <- getSegments polygon1; s2 <- getSegments polygon2]
    segmentIntersectionCandidateVertices = mapMaybe getSegmentIntersection segmentProduct
    candidateVertices = vertices1 ++ vertices2 ++ segmentIntersectionCandidateVertices

    resultVertices = filter (\v -> (inPolygon polygon1 v) && (inPolygon polygon2 v)) candidateVertices

{-
 - Given the (possibly unsorted) vertices of a (possibly degenerate) convex
 - polygon, returns their non-degenerate ConvexPolygons.
 -}
makeConvexPolygonFromVertices :: [Vec2] -> Maybe ConvexPolygon
makeConvexPolygonFromVertices (v1 : v2 : otherVertices)
  | isDegenerate = Nothing
  | otherwise = Just $ ConvexPolygon sortedVertices
  where
    vertices = v1 : v2 : otherVertices
    interiorPoint = (sum vertices) / (fromIntegral $ length vertices)
    isDegenerate = det22 (V2 (v1 - interiorPoint) (v2 - interiorPoint)) < 1e-5
    sortedVertices = sort (comparing vertexAngle) vertices
    vertexAngle vertex = atan2 y x where V2 x y = vertex - interiorPoint
makeConvexPolygonFromVertices _ = Nothing
