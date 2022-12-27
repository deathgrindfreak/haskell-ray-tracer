module RayTracer.Tuple
  ( Tuple(..)
  , cross
  , dot
  , magnitude
  , norm
  ) where

data Tuple a
  = Vec a a a
  | Point a a a
  | Scalar a
  deriving (Show, Eq)

instance Functor Tuple where
  fmap f (Vec a b c) = Vec (f a) (f b) (f c)
  fmap f (Point a b c) = Point (f a) (f b) (f c)
  fmap f (Scalar a) = Scalar (f a)

instance (RealFloat a) => Num (Tuple a) where
  {-# SPECIALIZE instance Num (Tuple Float) #-}
  {-# SPECIALIZE instance Num (Tuple Double) #-}

  negate = fmap negate

  Vec x1 y1 z1 + Vec x2 y2 z2 = Vec (x1 + x2) (y1 + y2) (z1 + z2)
  Point x1 y1 z1 + Vec x2 y2 z2 = Point (x1 + x2) (y1 + y2) (z1 + z2)
  Vec x1 y1 z1 + Point x2 y2 z2 = Point (x1 + x2) (y1 + y2) (z1 + z2)
  Point x1 y1 z1 + Point x2 y2 z2 = Vec (x1 + x2) (y1 + y2) (z1 + z2)
  Scalar a + Scalar b = Scalar (a + b)
  _ + _ = error "Unsupported addition"

  Scalar a * Vec x y z = Vec (a * x) (a * y) (a * z)
  Vec x y z * Scalar a = Vec (a * x) (a * y) (a * z)

  v1@Vec{} * v2@Vec{} = dot v1 v2
  Scalar a * Scalar b = Scalar (a * b)
  _ * _ = error "Unsupported multiplication"

  abs = Scalar . magnitude
  signum  = norm
  fromInteger = Scalar . fromInteger

{-# SPECIALIZE cross :: Tuple Double -> Tuple Double -> Tuple Double #-}
cross :: (RealFloat a) => Tuple a -> Tuple a -> Tuple a
cross (Vec x1 y1 z1) (Vec x2 y2 z2) =
  Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
cross _ _ = error "cross can only operate on vectors"

{-# SPECIALIZE dot :: Tuple Double -> Tuple Double -> Tuple Double #-}
dot :: (RealFloat a) => Tuple a -> Tuple a -> Tuple a
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = Scalar (x1 * x2 + y1 * y2 + z1 * z2)
dot _ _ = error "dot can only operate on vectors"

{-# SPECIALIZE magnitude :: Tuple Double -> Double #-}
magnitude :: (RealFloat a) => Tuple a -> a
magnitude (Vec x y z) = sqrt (x * x + y * y + z * z)
magnitude (Point x y z) = sqrt (x * x + y * y + z * z)
magnitude (Scalar a) = abs a

{-# SPECIALIZE norm :: Tuple Double -> Tuple Double #-}
norm :: (RealFloat a) => Tuple a -> Tuple a
norm v@(Vec x y z) = Vec (x/r) (y/r) (z/r) where r = magnitude v
norm v@(Point x y z) = Point (x/r) (y/r) (z/r) where r = magnitude v
norm (Scalar a) = Scalar (signum a)
