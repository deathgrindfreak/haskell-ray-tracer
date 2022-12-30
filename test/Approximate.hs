module Approximate (ApproxEq, Identity(..), shouldApproximate) where

import RayTracer.Tuple
import RayTracer.Matrix

import Test.Hspec
import qualified Data.Vector as V

infix 1 `shouldApproximate`

shouldApproximate :: (HasCallStack, Ord a, Floating a, ApproxEq f)
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

instance ApproxEq Matrix where
  approxEq M { elements = a } M { elements = b } =
    V.and $ V.zipWith (\x y -> approxEq (Identity x) (Identity y)) a b

instance ApproxEq Vec where
  approxEq (Vec a b c) (Vec d e f) =
    and $ zipWith (\x y -> approxEq (Identity x) (Identity y)) [a, b, c] [d, e, f]

instance ApproxEq Point where
  approxEq (Point a b c) (Point d e f) =
    and $ zipWith (\x y -> approxEq (Identity x) (Identity y)) [a, b, c] [d, e, f]

instance ApproxEq Scalar where
  approxEq (Scalar a) (Scalar b) = approxEq (Identity a) (Identity b)
