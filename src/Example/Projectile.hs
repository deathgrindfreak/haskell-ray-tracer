module Example.Projectile (run) where

import Data.List (unfoldr)
import qualified Data.Text.Lazy as T

import RayTracer.Canvas
import RayTracer.Color
import RayTracer.Tuple

data Projectile = Projectile
  { position :: Point Double
  , velocity :: Vec Double
  }
  deriving (Show)

data Environment = Environment
  { gravity :: Vec Double
  , wind :: Vec Double
  }
  deriving (Show)

run :: T.Text
run =
  let canvas = makeCanvas (900, 500)
      projectile =
        Projectile
          { position = Point 0 1 0
          , velocity = norm (Vec 1 1.8 0) |*| Scalar 11.25
          }
      environment =
        Environment
          { gravity = Vec 0 (-0.1) 0
          , wind = Vec (-0.02) 0 0
          }
      color = Color 1 0.8 0.6
      points = plotProjectile projectile environment
      flippedPoints = map (\(x, y) -> ((x, height canvas - y), color)) points
      squaredPoints = concatMap addNeighbors flippedPoints
   in canvasToPPM . (`writePixels` canvas) $ squaredPoints
  where
    addNeighbors ((x, y), c) = [((x + i, y + j), c) | i <- [-1 .. 1], j <- [-1 .. 1]]

plotProjectile :: Projectile -> Environment -> [(Int, Int)]
plotProjectile p e =
  unfoldr
    ( \projectile ->
        let p' = tick projectile e
            Point _ y _ = position p'
         in if y <= 0 then Nothing else Just (toPoint p', p')
    )
    p
  where
    toPoint pr = let Point x y _ = position pr in (round x, round y)

tick :: Projectile -> Environment -> Projectile
tick p@(Projectile pos v) (Environment g w) =
  p
    { position = pos |+| v
    , velocity = v |+| g |+| w
    }
