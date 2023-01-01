module Example.SimpleSphere (run) where

import           Data.Text.Lazy   (Text)
import           RayTracer.Canvas
import           RayTracer.Color
import           RayTracer.Ray
import           RayTracer.Tuple

run :: Text
run = canvasToPPM $ update determineColor canvas
  where
    canvasPixels = 100
    wallSize = 7.0
    half = wallSize / 2
    pixelSize = wallSize / fromIntegral canvasPixels

    canvas = makeCanvas (canvasPixels, canvasPixels)
    sphere = makeSphere 0
    rayOrigin = Point 0 0 (-5)

    determineColor ((x, y), color) =
      let wx = -half + pixelSize * fromIntegral x
          wy = half - pixelSize * fromIntegral y
          pos = Point wx wy 10
          ray :: Ray Double
          ray = Ray rayOrigin (norm (pos |-| rayOrigin))
          iss = sphere `intersect` ray
      in ((x, y), if null iss then color else Color 1 0 0)
