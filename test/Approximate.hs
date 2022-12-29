module Approximate (ApproxEq, Identity(..), shouldApproximate) where

import RayTracer.Tuple

import Test.Hspec

shouldApproximate :: (HasCallStack, Show a, Eq a, Ord a, Floating a, ApproxEq f)
                  => f a -> f a -> Expectation
shouldApproximate a b = (a `approxEq` b) `shouldBe` True

epsilon :: Floating a => a
epsilon = 1e-5

class ApproxEq f where
  {-# MINIMAL approxEq #-}
  approxEq :: (Floating a, Ord a) => f a -> f a -> Bool

newtype Identity a = Identity { runIdentity :: a }
  deriving (Show)

instance ApproxEq Identity where
  approxEq (Identity a) (Identity b) = abs (a - b) < epsilon

instance ApproxEq Tuple where
  approxEq (Point a b c) (Point d e f) =
    and $ zipWith (\x y -> approxEq (Identity x) (Identity y)) [a, b, c] [d, e, f]
  approxEq (Vec a b c) (Vec d e f) =
    and $ zipWith (\x y -> approxEq (Identity x) (Identity y)) [a, b, c] [d, e, f]
  approxEq (Scalar a) (Scalar b) = approxEq (Identity a) (Identity b)
