{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module RayTracer.Ray
  ( Ray (..)
  , origin
  , direction
  , Object (..)
  , Shape (..)
  , objectId
  , transform
  , material
  , shape
  , defaultShape
  , NoId
  , HasId
  , position
  , Intersection (..)
  , object
  , t
  , Intersections
  , addIntersections
  , intersections
  , hit
  , intersect
  , localRay
  , normalAt
  , normalAtFor
  , localNormalAt
  ) where

import Control.Lens hiding (transform)
import qualified RayTracer.Heap as H
import RayTracer.Light (Material, defaultMaterial)
import RayTracer.Matrix
import RayTracer.Transform
import RayTracer.Tuple
import RayTracer.Util

import Data.Function (on)

data Ray a = Ray
  { _origin :: Point a
  , _direction :: Vec a
  }
  deriving (Eq, Show)

data Shape = Sphere | Plane
  deriving (Eq, Show)

data Object objectId = Object
  { _objectId :: objectId
  , _shape :: Shape
  , _transform :: Transform Double
  , _material :: Material
  }
  deriving (Eq, Show)

type NoId = ()
type HasId = Int

data Intersection = Intersection
  { _object :: Object HasId
  , _t :: Double
  }
  deriving (Show)

makeLenses ''Ray
makeLenses ''Object
makeLenses ''Intersection

instance VecMult Ray Transform Ray where
  Ray o d |*| tr = Ray (o |*| tr) (d |*| tr)

instance VecMult Transform Ray Ray where
  tr |*| Ray o d = Ray (tr |*| o) (tr |*| d)

defaultShape :: Shape -> Object NoId
defaultShape s =
  Object
    { _objectId = ()
    , _shape = s
    , _transform = identityTransform
    , _material = defaultMaterial
    }

instance Eq Intersection where
  (==) = on (==) (^. t)

instance Ord Intersection where
  compare = on compare (^. t)

position :: (Num a, Eq a) => Ray a -> a -> Point a
position r t' = (r ^. origin) |+| (r ^. direction) |*| Scalar t'

localRay :: Object HasId -> Ray Double -> Ray Double
localRay o r = inverse (o ^. transform) |*| r

intersect :: Object HasId -> Ray Double -> [Intersection]
intersect o r =
  let ray = localRay o r
   in case o ^. shape of
        Sphere ->
          let sphereToRay = (ray ^. origin) |-| Point 0 0 0
              a = fromScalar $ (ray ^. direction) |*| (ray ^. direction)
              b = fromScalar $ Scalar 2 |*| ray ^. direction |*| sphereToRay
              c = fromScalar $ sphereToRay |*| sphereToRay |-| Scalar 1
              d = b * b - 4 * a * c
           in if d < 0
                then []
                else
                  let t1 = (-b - sqrt d) / (2 * a)
                      t2 = (-b + sqrt d) / (2 * a)
                   in map (Intersection o) [t1, t2]
        Plane ->
          if abs (r ^. direction . vy) < epsilon
            then []
            else
              let t' = (-r ^. origin . py) / (r ^. direction . vy)
               in [Intersection o t']

localNormalAt :: Object HasId -> Point Double -> Vec Double
localNormalAt o lp =
  case o ^. shape of
    Sphere -> lp |-| Point 0 0 0
    Plane -> Vec 0 1 0

normalAt :: Object HasId -> Point Double -> Vec Double
normalAt o p = normalAtFor o p (localNormalAt o)

normalAtFor ::
  Object HasId ->
  Point Double ->
  (Point Double -> Vec Double) ->
  Vec Double
normalAtFor o p localNormAt =
  let localPoint = inverse (o ^. transform) |*| p
      localNormal = localNormAt localPoint
      worldNormal = transpose (inverse (o ^. transform)) |*| localNormal
   in norm worldNormal

type Intersections = H.LeftistHeap Intersection

addIntersections :: Intersections -> [Intersection] -> Intersections
addIntersections h = foldr H.insert h . filter ((>= 0) . (^. t))

intersections :: [Intersection] -> Intersections
intersections = addIntersections H.empty

hit :: Intersections -> Maybe Intersection
hit = H.findMin
