{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RayTracer.World
  ( World (..)
  , light
  , objects
  , mkWorld
  , intersectWorld
  , Computation (..)
  , intersection
  , point
  , overPoint
  , eyev
  , normalv
  , inside
  , prepareComputations
  , shadeHit
  , colorAt
  , isShadowed
  , epsilon
  )
where

import Control.Lens hiding (inside)
import qualified Data.List as List
import qualified Data.Vector as V
import RayTracer.Color
import qualified RayTracer.Light as L
import qualified RayTracer.Ray as R
import RayTracer.Tuple

epsilon :: Double
epsilon = 1e-5

data World = World
  { _light :: L.PointLight
  , _objects :: V.Vector (R.Object R.HasId)
  }
  deriving (Show)

data Computation = Computation
  { _intersection :: R.Intersection
  , _point :: Point Double
  , _overPoint :: Point Double
  , _eyev :: Vec Double
  , _normalv :: Vec Double
  , _inside :: Bool
  }
  deriving (Show)

makeLenses ''World
makeLenses ''Computation

mkWorld :: L.PointLight -> [R.Object R.NoId] -> World
mkWorld l objs =
  World
    { _light = l
    , _objects = V.imap (\oId o -> o & R.objectId .~ oId) (V.fromList objs)
    }

intersectWorld :: R.Ray Double -> World -> [R.Intersection]
intersectWorld ray =
  List.sortOn (^. R.t) . V.foldr (\x a -> a ++ R.intersect x ray) [] . (^. objects)

prepareComputations :: R.Intersection -> R.Ray Double -> Computation
prepareComputations i ray =
  let pt = R.position ray (i ^. R.t)
      nv = R.normalAt (i ^. R.object) pt
      ev = neg $ ray ^. R.direction
      insideObj = nv `dot` ev < 0
      nv' = if insideObj then neg nv else nv
   in Computation
        { _intersection = i
        , _point = pt
        , _overPoint = pt |+| nv' |*| Scalar epsilon
        , _eyev = ev
        , _normalv = nv'
        , _inside = insideObj
        }

shadeHit :: World -> Computation -> Color Double
shadeHit w c =
  L.lighting
    (c ^. intersection . R.object . R.material)
    (w ^. light)
    (c ^. point)
    (c ^. eyev)
    (c ^. normalv)
    (isShadowed w (c ^. overPoint))

colorAt :: World -> R.Ray Double -> Color Double
colorAt world ray =
  let is = R.intersections (intersectWorld ray world)
   in case R.hit is of
        Just h -> shadeHit world (prepareComputations h ray)
        Nothing -> Color 0 0 0

isShadowed :: World -> Point Double -> Bool
isShadowed w p =
  let v = w ^. light . L.position |-| p
      distance = magnitude v
      direction = norm v
      r = R.Ray p direction
      is = intersectWorld r w
      h = R.hit $ R.intersections is
   in maybe False ((< distance) . (^. R.t)) h
