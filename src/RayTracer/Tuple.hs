{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}

module RayTracer.Tuple
  ( Vec(..)
  , Point(..)
  , Scalar(..)
  , VecAdd((|+|))
  , VecSub((|-|))
  , VecMult((|*|))
  , cross
  , dot
  , magnitude
  , norm
  ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers (EqProp, eq, (=-=))
import Test.QuickCheck (Arbitrary(arbitrary))

infixl 7 |*|
infixl 6 |-|
infixl 6 |+|

data Vec a = Vec !a !a !a deriving (Show, Eq, Functor)
data Point a = Point !a !a !a deriving (Show, Eq, Functor)
newtype Scalar a = Scalar a deriving (Show, Eq)

instance Applicative Vec where
  pure a = Vec a a a
  Vec f g h <*> Vec a b c = Vec (f a) (g b) (h c)

instance Applicative Point where
  pure a = Point a a a
  Point f g h <*> Point a b c = Point (f a) (g b) (h c)

class VecAdd v w z | v w -> z where
  (|+|) :: Num a => v a -> w a -> z a

instance VecAdd Vec Vec Vec where
  Vec x1 y1 z1 |+| Vec x2 y2 z2 = Vec (x1 + x2) (y1 + y2) (z1 + z2)

instance VecAdd Point Vec Point where
  Point x1 y1 z1 |+| Vec x2 y2 z2 = Point (x1 + x2) (y1 + y2) (z1 + z2)

instance VecAdd Vec Point Point where
  Vec x1 y1 z1 |+| Point x2 y2 z2 = Point (x1 + x2) (y1 + y2) (z1 + z2)

class VecSub v w z | v w -> z where
  (|-|) :: Num a => v a -> w a -> z a

instance VecSub Point Point Vec where
  Point x1 y1 z1 |-| Point x2 y2 z2 = Vec (x1 - x2) (y1 - y2) (z1 - z2)

instance VecSub Vec Vec Vec where
  Vec x1 y1 z1 |-| Vec x2 y2 z2 = Vec (x1 - x2) (y1 - y2) (z1 - z2)

class VecMult v w z | v w -> z where
  (|*|) :: (Num a, Eq a) => v a -> w a -> z a

instance VecMult Vec Vec Scalar where
  v1 |*| v2 = Scalar $ v1 `dot` v2

instance VecMult Scalar Vec Vec where
  Scalar a |*| Vec b c d = Vec (a * b) (a * c) (a * d)

instance VecMult Vec Scalar Vec where
  Vec b c d |*| Scalar a = Vec (a * b) (a * c) (a * d)

instance VecMult Scalar Point Point where
  Scalar a |*| Point b c d = Point (a * b) (a * c) (a * d)

instance VecMult Point Scalar Point where
  Point b c d |*| Scalar a = Point (a * b) (a * c) (a * d)

{-# SPECIALIZE cross :: Vec Double -> Vec Double -> Vec Double #-}
cross :: (Num a) => Vec a -> Vec a -> Vec a
cross (Vec x1 y1 z1) (Vec x2 y2 z2) =
  Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

{-# SPECIALIZE dot :: Vec Double -> Vec Double -> Double #-}
dot :: (Num a) => Vec a -> Vec a -> a
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

{-# SPECIALIZE magnitude :: Vec Double -> Double #-}
magnitude :: (RealFloat a) => Vec a -> a
magnitude (Vec x y z) = sqrt (x * x + y * y + z * z)

{-# SPECIALIZE norm :: Vec Double -> Vec Double #-}
norm :: (RealFloat a) => Vec a -> Vec a
norm v@(Vec x y z) = Vec (x/r) (y/r) (z/r) where r = magnitude v

instance Arbitrary a => Arbitrary (Point a) where
  arbitrary = Point <$> arbitrary <*> arbitrary <*> arbitrary

instance Eq a => EqProp (Point a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Vec a) where
  arbitrary = Vec <$> arbitrary <*> arbitrary <*> arbitrary

instance Eq a => EqProp (Vec a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Scalar a) where
  arbitrary = Scalar <$> arbitrary

instance Eq a => EqProp (Scalar a) where
  (=-=) = eq
