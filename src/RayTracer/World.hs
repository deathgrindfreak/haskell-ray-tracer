{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracer.World
  ( World (..)
  , intersectWorld
  , Computation (..)
  , prepareComputations
  , shadeHit
  , colorAt
  )
where

import qualified Data.List as List
import qualified Data.Vector as V
import RayTracer.Color
import qualified RayTracer.Light as L
import qualified RayTracer.Ray as Ray
import RayTracer.Tuple

data World = World
  { light :: L.PointLight
  , objects :: V.Vector Ray.Object
  }
  deriving (Show)

intersectWorld :: Ray.Ray Double -> World -> [Ray.Intersection]
intersectWorld ray =
  List.sortOn Ray.t . V.foldr (\x a -> a ++ Ray.intersect x ray) [] . objects

data Computation = Computation
  { t :: Double
  , object :: Ray.Object
  , point :: Point Double
  , eyev :: Vec Double
  , normalv :: Vec Double
  , inside :: Bool
  }
  deriving (Show)

prepareComputations :: Ray.Intersection -> Ray.Ray Double -> Computation
prepareComputations i ray =
  let point = Ray.position ray (Ray.t i)
      normalv = Ray.normalAt (Ray.object i) point
      eyev = neg $ Ray.direction ray
      inside = normalv `dot` eyev < 0
   in Computation
        { t = Ray.t i
        , object = Ray.object i
        , point
        , eyev
        , normalv =
            if inside
              then neg normalv
              else normalv
        , inside
        }

shadeHit :: World -> Computation -> Color Double
shadeHit World {light} Computation {..} =
  L.lighting (Ray.material object) light point eyev normalv

colorAt :: World -> Ray.Ray Double -> Color Double
colorAt world ray =
  let is = Ray.intersections (intersectWorld ray world)
   in case Ray.hit is of
        Just h -> shadeHit world (prepareComputations h ray)
        Nothing -> Color 0 0 0
