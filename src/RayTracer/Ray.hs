{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module RayTracer.Ray
  ( Ray (..)
  , Object (..)
  , makeSphere
  , position
  , Intersection (..)
  , intersections
  , hit
  , intersect
  , normalAt
  ) where

import qualified RayTracer.Heap as H
import RayTracer.Light
import RayTracer.Matrix
import RayTracer.Tuple

import Data.Function (on)

data Ray a = Ray
  { origin :: Point a
  , direction :: Vec a
  }
  deriving (Eq, Show)

instance VecMult Ray Transform Ray where
  Ray o d |*| t = Ray (o |*| t) (d |*| t)

instance VecMult Transform Ray Ray where
  t |*| Ray o d = Ray (t |*| o) (t |*| d)

data Object = Sphere
  { objectId :: Int
  , transform :: Transform Double
  , material :: Material
  }
  deriving (Eq, Show)

makeSphere :: Int -> Object
makeSphere objectId =
  Sphere
    { objectId
    , transform = identityTransform
    , material = defaultMaterial
    }

data Intersection = Intersection
  { object :: Object
  , t :: Double
  }
  deriving (Show)

instance Eq Intersection where
  (==) = on (==) t

instance Ord Intersection where
  compare = on compare t

position :: (Num a, Eq a) => Ray a -> a -> Point a
position Ray {origin, direction} t = origin |+| direction |*| Scalar t

intersect :: Object -> Ray Double -> [Intersection]
intersect o@Sphere {transform} r =
  let ray = inverse transform |*| r
      sphereToRay = origin ray |-| Point 0 0 0
      a = fromScalar $ direction ray |*| direction ray
      b = fromScalar $ Scalar 2 |*| direction ray |*| sphereToRay
      c = fromScalar $ sphereToRay |*| sphereToRay |-| Scalar 1
      d = b * b - 4 * a * c
   in if d < 0
        then []
        else
          let t1 = (-b - sqrt d) / (2 * a)
              t2 = (-b + sqrt d) / (2 * a)
           in map (Intersection o) [t1, t2]

normalAt :: Object -> Point Double -> Vec Double
normalAt Sphere {transform} p =
  let objectPoint = inverse transform |*| p
      objectNormal = objectPoint |-| Point 0 0 0
      worldNormal = transpose (inverse transform) |*| objectNormal
   in norm worldNormal

intersections ::
  H.LeftistHeap Intersection ->
  [Intersection] ->
  H.LeftistHeap Intersection
intersections h = foldr H.insert h . filter ((>= 0) . t)

hit :: H.LeftistHeap Intersection -> Maybe Intersection
hit = H.findMin
