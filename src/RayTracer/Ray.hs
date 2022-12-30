{-# LANGUAGE NamedFieldPuns #-}

module RayTracer.Ray
  ( Ray(..)
  , Sphere(..)
  , position
  , intersect
  ) where

import RayTracer.Tuple

import qualified Data.HashSet as S

data Ray a = Ray
  { origin :: Point a
  , direction :: Vec a
  }
  deriving (Show)

position :: (Num a, Eq a) => Ray a -> a -> Point a
position Ray { origin, direction } t = origin |+| direction |*| Scalar t

data Sphere a = Sphere
  { id :: a
  }
  deriving (Show)

intersect :: Num a => Sphere a -> Ray a -> [a]
intersect = undefined
