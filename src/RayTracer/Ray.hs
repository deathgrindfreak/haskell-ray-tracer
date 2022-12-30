{-# LANGUAGE NamedFieldPuns #-}

module RayTracer.Ray
  ( Ray(..)
  , position
  ) where

import RayTracer.Tuple

data Ray = Ray
  { origin :: Point Double
  , direction :: Vec Double
  }
  deriving (Show)

position :: Ray -> Double -> Point Double
position Ray { origin, direction } t = origin |+| direction |*| Scalar t
