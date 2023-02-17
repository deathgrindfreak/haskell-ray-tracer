{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracer.World
  ( World (..)
  , defaultWorld
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
import RayTracer.Matrix
import qualified RayTracer.Ray as Ray
import RayTracer.Tuple

data World = World
  { light :: L.PointLight
  , objects :: V.Vector Ray.Object
  }
  deriving (Show)

defaultWorld :: World
defaultWorld =
  let materialLarger =
        L.defaultMaterial
          { L.materialColor = Color 0.8 1.0 0.6
          , L.diffuse = 0.7
          , L.specular = 0.2
          }
   in World
        { light = L.PointLight (Point (-10) 10 (-10)) (Color 1 1 1)
        , objects =
            V.fromList
              [ (Ray.makeSphere 0) {Ray.material = materialLarger}
              , (Ray.makeSphere 1) {Ray.transform = scaling 0.5 0.5 0.5}
              ]
        }

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
      eyev = Scalar (-1) |*| Ray.direction ray
      inside = normalv `dot` eyev < 0
   in Computation
        { t = Ray.t i
        , object = Ray.object i
        , point
        , eyev
        , normalv =
            if inside
              then Scalar (-1) |*| normalv
              else normalv
        , inside
        }

shadeHit :: World -> Computation -> Color Double
shadeHit World {light} Computation {..} =
  L.lighting (Ray.material object) light point eyev normalv

colorAt :: World -> Ray.Ray Double -> Color Double
colorAt world ray =
  case intersectWorld ray world of
    (h : _) -> shadeHit world (prepareComputations h ray)
    [] -> Color 0 0 0
