module Wave where

import Data.Vector
  ( Vector
  , generate
  )

import Linear
  ( V2(V2)
  )

import Geometry2.Geodesic (advanceGeodesic)

import Geometry2.Primitives
  ( DiscreteSurface
  , FaceBucketIndex(FaceBucketIndex)
  , FaceIndex(FaceIndex)
  , FaceField(FaceField)
  , SurfaceBucketCoordinate(SurfaceBucketCoordinate)
  , SurfaceField(SurfaceField)
  , zipSurfaceFieldWith
  , lookupFaceField
  , values
  , bucketCount
  , bucketCenter
  , readBucketFromSurfaceField
  , toSurfaceBucketCoordinate
  , bFace
  , fineness
  )

data WaveState = WaveState
  { value :: SurfaceField Vector Double
  , velocity :: SurfaceField Vector Double
  }

-- Wave equation says that u_tt = u_xx + u_yy
advance :: Double -> DiscreteSurface -> WaveState -> WaveState
advance dt surface (WaveState value velocity) =
  WaveState newValue newVelocity
  where
    newValue = zipSurfaceFieldWith (\pointValue pointVelocity -> pointValue + dt * pointVelocity) value velocity
    newVelocity = zipSurfaceFieldWith (\pointVelocity pointAcceleration -> pointVelocity + dt * pointAcceleration) velocity (laplacian surface value)

laplacian :: DiscreteSurface -> SurfaceField Vector Double -> SurfaceField Vector Double
laplacian surface surfaceField = SurfaceField $
  generate (length faceFields) (\i -> faceFieldLaplacian surface surfaceField (FaceIndex i))
  where
    SurfaceField faceFields = surfaceField

faceFieldLaplacian :: DiscreteSurface -> SurfaceField Vector Double -> FaceIndex -> FaceField Vector Double
faceFieldLaplacian surface surfaceField faceIndex =
    faceField
      { values = generate
        (bucketCount faceField)
        (\index ->
          pointLaplacian
            surface
            surfaceField
            (SurfaceBucketCoordinate faceIndex (FaceBucketIndex index)))
      }
  where
    faceField = lookupFaceField surfaceField faceIndex

pointLaplacian :: DiscreteSurface -> SurfaceField Vector Double -> SurfaceBucketCoordinate -> Double
pointLaplacian surface field surfaceBucketCoordinate = atPoint
  where
    faceField = lookupFaceField field (bFace surfaceBucketCoordinate)
    epsilon = fineness faceField

    mayPoint = bucketCenter surface field surfaceBucketCoordinate

    atPoint = getValue (V2 0 0)
    atPlusX = getValue (V2 1 0)
    atMinusX = getValue (V2 (-1) 0)
    atPlusY = getValue (V2 0 1)
    atMinusY = getValue (V2 0 (-1))

    u_xx = (atPlusX - atPoint) - (atPoint - atMinusX) / (epsilon * epsilon)
    u_yy = (atPlusY - atPoint) - (atPoint - atMinusY) / (epsilon * epsilon)

    getValue direction = case mayPoint of
      Just point ->
        readBucketFromSurfaceField
          field
          (toSurfaceBucketCoordinate
            field
            (advanceGeodesic surface point direction epsilon))
      _ -> 0
