module RayTracer.Color
  ( Color(..)
  ) where

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers (EqProp, eq, (=-=))
import Control.Applicative (Applicative(liftA2))

data Color a = Color
  { red :: !a
  , green :: !a
  , blue :: !a
  }
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Color a) where
  arbitrary = Color <$> arbitrary <*> arbitrary <*> arbitrary

instance Eq a => EqProp (Color a) where
  (=-=) = eq

instance Functor Color where
  fmap f (Color a b c) = Color (f a) (f b) (f c)

instance Applicative Color where
  pure a = Color a a a
  (Color f g h) <*> (Color a b c) = Color (f a) (g b) (h c)

instance (Num a) =>  Num (Color a) where
  {-# SPECIALIZE instance Num (Color Float) #-}
  {-# SPECIALIZE instance Num (Color Double) #-}

  negate = fmap negate
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  fromInteger i = Color i' i' i' where i' = fromInteger i
  abs = fmap abs
  signum = fmap signum
