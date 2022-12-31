module Example.SimpleSphere (run) where

import           Data.Maybe       (fromMaybe, isJust)
import           Data.Text.Lazy   (Text)
import           RayTracer.Canvas
import           RayTracer.Color
import           RayTracer.Ray
import           RayTracer.Tuple

run :: Text
run =
  let points = sequence . filter isJust
        $ [determineColor x y | x <- [0..canvasPixels-1], y <- [0..canvasPixels-1]]
  in canvasToPPM . (`writePixels` canvas) $ fromMaybe [] points

  where
    canvasPixels = 100
    wallSize = 7.0
    half = wallSize / 2
    pixelSize = wallSize / fromIntegral canvasPixels

    canvas = makeCanvas (canvasPixels, canvasPixels)
    sphere = makeSphere 0
    rayOrigin = Point 0 0 (-5)

    determineColor :: Int -> Int -> Maybe ((Int, Int), Color Double)
    determineColor x y =
      let (wx, wy) = (-half + pixelSize * fromIntegral x, half - pixelSize * fromIntegral y)
          pos = Point wx wy 10
          ray :: Ray Double
          ray = Ray rayOrigin (norm (pos |-| rayOrigin))
          iss = sphere `intersect` ray
      in if null iss then Nothing else Just ((x, y), Color 1 0 0)
