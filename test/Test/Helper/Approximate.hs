module Test.Helper.Approximate
  ( ApproxEq
  , Approx (..)
  , shouldApproximate
  , (~==)
  )
where

import RayTracer.Color
import RayTracer.Matrix
import RayTracer.Ray
import RayTracer.Tuple

import qualified Data.Vector as V
import Hedgehog (MonadTest, (===))
import Test.Hspec

infix 1 `shouldApproximate`

shouldApproximate ::
  (HasCallStack, Ord a, Floating a, ApproxEq f) =>
  f a ->
  f a ->
  Expectation
shouldApproximate a b = (a `approxEq` b) `shouldBe` True

infix 4 ~==

(~==) ::
  (MonadTest m, HasCallStack, Ord a, Floating a, ApproxEq f) =>
  f a ->
  f a ->
  m ()
a ~== b = (a `approxEq` b) === True

epsilon :: Floating a => a
epsilon = 1e-5

class ApproxEq f where
  {-# MINIMAL approxEq #-}
  approxEq :: (Floating a, Ord a) => f a -> f a -> Bool

newtype Approx a = Approx {runApprox :: a}
  deriving (Show)

instance ApproxEq Approx where
  approxEq (Approx a) (Approx b) = abs (a - b) < epsilon

instance ApproxEq Matrix where
  approxEq M {elements = a} M {elements = b} =
    V.and $ V.zipWith (\x y -> approxEq (Approx x) (Approx y)) a b

instance ApproxEq Vec where
  approxEq (Vec a b c) (Vec d e f) =
    and $ zipWith (\x y -> approxEq (Approx x) (Approx y)) [a, b, c] [d, e, f]

instance ApproxEq Point where
  approxEq (Point a b c) (Point d e f) =
    and $ zipWith (\x y -> approxEq (Approx x) (Approx y)) [a, b, c] [d, e, f]

instance ApproxEq Color where
  approxEq (Color a b c) (Color d e f) =
    and $ zipWith (\x y -> approxEq (Approx x) (Approx y)) [a, b, c] [d, e, f]

instance ApproxEq Scalar where
  approxEq (Scalar a) (Scalar b) = approxEq (Approx a) (Approx b)

instance ApproxEq Ray where
  approxEq (Ray o1 d1) (Ray o2 d2) = approxEq o1 o2 && approxEq d1 d2
