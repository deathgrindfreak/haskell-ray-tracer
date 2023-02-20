{-# LANGUAGE NamedFieldPuns #-}

module Example.ShadedSphere (run) where

import Control.Lens
import Data.Text.Lazy (Text)
import RayTracer.Canvas
import RayTracer.Color
import RayTracer.Light hiding (position)
import RayTracer.Ray
import RayTracer.Tuple

run :: Text
run = canvasToPPM $ mapCanvas determineColor canvas
  where
    canvasPixels = 500
    wallSize = 7.0
    half = wallSize / 2
    pixelSize = wallSize / fromIntegral canvasPixels

    canvas = makeCanvas (canvasPixels, canvasPixels)
    sphereMaterial = defaultMaterial & color .~ Color 1 0.2 1

    sphere =
      defaultSphere
        & objectId .~ 0
        & material .~ sphereMaterial

    light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)

    rayOrigin = Point 0 0 (-5)

    determineColor ((x, y), c) =
      let wx = -half + pixelSize * fromIntegral x
          wy = half - pixelSize * fromIntegral y
          pos = Point wx wy 10
          ray :: Ray Double
          ray = Ray rayOrigin (norm (pos |-| rayOrigin))
          iss = sphere `intersect` ray
          inter = hit (intersections iss)

          newColor = case inter of
            Nothing -> c
            Just i ->
              let point = position ray (i ^. t)
                  normal = normalAt (i ^. object) point
                  eye = neg (ray ^. direction)
               in lighting (i ^. object ^. material) light point eye normal
       in ((x, y), newColor)
