{-# LANGUAGE NamedFieldPuns #-}

module Example.ShadedSphere (run) where

import           Data.Text.Lazy   (Text)
import           RayTracer.Canvas
import           RayTracer.Color
import qualified RayTracer.Heap   as H
import           RayTracer.Light
import           RayTracer.Ray
import           RayTracer.Tuple

run :: Text
run = canvasToPPM $ mapCanvas determineColor canvas
  where
    canvasPixels = 500
    wallSize = 7.0
    half = wallSize / 2
    pixelSize = wallSize / fromIntegral canvasPixels

    canvas = makeCanvas (canvasPixels, canvasPixels)
    sphereMaterial = defaultMaterial { materialColor = Color 1 0.2 1 }
    sphere = (makeSphere 0) { material = sphereMaterial }
    light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)

    rayOrigin = Point 0 0 (-5)

    determineColor ((x, y), color) =
      let wx = -half + pixelSize * fromIntegral x
          wy = half - pixelSize * fromIntegral y
          pos = Point wx wy 10
          ray :: Ray Double
          ray = Ray rayOrigin (norm (pos |-| rayOrigin))
          iss = sphere `intersect` ray
          inter = hit (intersections H.empty iss)

          newColor = case inter of
            Nothing -> color
            Just Intersection { object, t = tVal } ->
              let point = position ray tVal
                  normal = normalAt object point
                  eye = Scalar (-1) |*| direction ray
              in lighting (material object) light point eye normal

      in ((x, y), newColor)
