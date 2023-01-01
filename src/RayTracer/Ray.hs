{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module RayTracer.Ray
  ( Ray(..)
  , Sphere(..)
  , makeSphere
  , position
  , Object(..)
  , Intersection(..)
  , intersections
  , hit
  ) where

import qualified RayTracer.Heap   as H
import           RayTracer.Light
import           RayTracer.Matrix
import           RayTracer.Tuple

import           Data.Function    (on)

data Ray a
  = Ray
      { origin    :: Point a
      , direction :: Vec a
      }
  deriving (Eq, Show)

instance VecMult Ray Transform Ray where
  Ray o d |*| t = Ray (o |*| t) (d |*| t)

instance VecMult Transform Ray Ray where
  t |*| Ray o d = Ray (t |*| o) (t |*| d)

data Sphere a
  = Sphere
      { sphereId       :: Int
      , transform      :: Transform a
      , sphereMaterial :: Material
      }
  deriving (Show)

makeSphere :: Num a => Int -> Sphere a
makeSphere sphereId =
  Sphere { sphereId
         , transform = identityTransform
         , sphereMaterial = defaultMaterial
         }

data Intersection a b
  = Intersection
      { object :: a
      , t      :: b
      }
  deriving (Show)

instance Eq b => Eq (Intersection a b) where
  (==) = on (==) t

instance Ord b => Ord (Intersection a b) where
  compare = on compare t

position :: (Num a, Eq a) => Ray a -> a -> Point a
position Ray { origin, direction } t = origin |+| direction |*| Scalar t

class Object o where
  objectId :: o a -> Int
  material :: o a -> Material
  intersect :: RealFloat a => o a -> Ray a -> [Intersection (o a) a]
  normalAt :: RealFloat a => o a -> Point a -> Vec a

instance Object Sphere where
  objectId = sphereId
  material = sphereMaterial

  normalAt Sphere { transform } p =
    let objectPoint = inverse transform |*| p
        objectNormal = objectPoint |-| Point 0 0 0
        worldNormal = transpose (inverse transform) |*| objectNormal
     in norm worldNormal

  intersect o@Sphere { transform } r =
    let ray = inverse transform |*| r
        sphereToRay = origin ray |-| Point 0 0 0
        a = fromScalar $ direction ray |*| direction ray
        b = fromScalar $ Scalar 2 |*| direction ray |*| sphereToRay
        c = fromScalar $ sphereToRay |*| sphereToRay |-| Scalar 1
        d = b * b - 4 * a * c
     in if d < 0
        then []
        else let t1 = (-b - sqrt d) / (2 * a)
                 t2 = (-b + sqrt d) / (2 * a)
             in map (Intersection o) [t1, t2]

intersections :: (Ord b, Num b)
              => H.LeftistHeap (Intersection a b)
              -> [Intersection a b]
              -> H.LeftistHeap (Intersection a b)
intersections h = foldr H.insert h . filter ((>= 0) . t)

hit :: Ord b => H.LeftistHeap (Intersection a b) -> Maybe (Intersection a b)
hit = H.findMin
