{-# LANGUAGE NamedFieldPuns #-}

module RayTracer.Ray
  ( Ray(..)
  , Sphere(..)
  , position
  , Object(..)
  , Intersectable(..)
  , Intersection(..)
  , intersections
  , hit
  ) where

import RayTracer.Tuple
import qualified RayTracer.Heap as H

import Data.Function (on)

data Ray a = Ray
  { origin :: Point a
  , direction :: Vec a
  }
  deriving (Show)

data Object o = Object
  { objectId :: Int
  , object :: o
  }
  deriving (Show)

instance Eq (Object o) where
  (==) = on (==) objectId

instance Ord (Object o) where
  compare = on compare objectId

data Sphere = Sphere deriving (Show)

data Intersection a b = Intersection
  { hitObject :: Object a
  , t :: b
  }
  deriving (Show)

instance Eq b => Eq (Intersection a b) where
  (==) = on (==) t

instance Ord b => Ord (Intersection a b) where
  compare = on compare t

position :: (Num a, Eq a) => Ray a -> a -> Point a
position Ray { origin, direction } t = origin |+| direction |*| Scalar t

class Intersectable o where
  intersect :: RealFloat a => Object o -> Ray a -> [Intersection o a]

instance Intersectable Sphere where
  intersect o ray =
    let sphereToRay = origin ray |-| Point 0 0 0
        a = fromScalar $ direction ray |*| direction ray
        b = fromScalar $ Scalar 2 |*| direction ray |*| sphereToRay
        c = fromScalar $ sphereToRay |*| sphereToRay |-| Scalar 1
        d = b * b - 4 * a * c
     in if d < 0
        then []
        else let t1 = (-b - sqrt d) / (2 * a)
                 t2 = (-b + sqrt d) / (2 * a)
             in map (Intersection o) [t1, t2]

intersections :: (H.Heap h, Ord b, Num b)
              => h (Intersection a b)
              -> [Intersection a b]
              -> h (Intersection a b)
intersections h = foldr H.insert h . filter ((>= 0) . t)

hit :: (H.Heap h, Ord b) => h (Intersection a b) -> Maybe (Intersection a b)
hit = H.findMin
