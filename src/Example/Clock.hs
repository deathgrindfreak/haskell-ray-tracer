module Example.Clock (run) where

import RayTracer.Canvas
import RayTracer.Color
import RayTracer.Tuple
import RayTracer.Matrix
import Data.Text.Lazy (Text)

run :: Text
run =
  let side :: Int
      side = 300
      center = fromIntegral side / 2
      radius = 100
      canvas = makeCanvas (side, side)

      threeOClock :: Tuple Double
      threeOClock = Point radius 0 0

      clockPoints = take 12 $ iterate (rotationZ (pi / 6) |*>) threeOClock
      centeredPoints = map (\a -> translation center center 0 |*> a) clockPoints
      points = map (\(Point x y _) -> ((round x, round y), Color 1 0.8 0.6)) centeredPoints
  in canvasToPPM . (`writePixels` canvas) $ points
