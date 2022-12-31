module RaySpec (spec) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec.QuickCheck
import SpecHelper

import RayTracer.Ray
import RayTracer.Tuple
import qualified RayTracer.Heap as H

spec :: Spec
spec = describe "Ray" $ do
  it "Position" $ do
    let r = Ray (Point 2 3 4) (Vec 1 0 0)
    position r 0 `shouldBe` Point 2 3 4
    position r 1 `shouldBe` Point 3 3 4
    position r (-1) `shouldBe` Point 1 3 4
    position r 2.5 `shouldBe` Point 4.5 3 4

  it "Intersect ray through sphere" $ do
    let r :: Ray Double
        r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
        s = Object 0 Sphere
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [4.0, 6.0]

  it "Intersect ray tangent to sphere" $ do
    let r = Ray (Point 0 1 (-5)) (Vec 0 0 1)
        s = Object 0 Sphere
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [5.0, 5.0]

  it "Intersect ray misses sphere" $ do
    let r = Ray (Point 0 2 (-5)) (Vec 0 0 1)
        s = Object 0 Sphere
        xs = s `intersect` r
    xs `shouldBe` []

  it "Intersect ray inside of sphere" $ do
    let r = Ray (Point 0 0 0) (Vec 0 0 1)
        s = Object 0 Sphere
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [-1.0, 1.0]

  it "Intersect ray ahead of sphere" $ do
    let r = Ray (Point 0 0 5) (Vec 0 0 1)
        s = Object 0 Sphere
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [-6.0, -4.0]

  it "Hit all intersections have positive t" $ do
    let s = Object 0 Sphere
        i1 = Intersection s 1
        i2 = Intersection s 2
        xs :: H.LeftistHeap (Intersection Sphere Int)
        xs = intersections H.empty [i1, i2]
    hit xs `shouldBe` Just i1

  it "Hit when some intersections have negative t" $ do
    let s = Object 0 Sphere
        i1 = Intersection s (-1)
        i2 = Intersection s 2
        xs :: H.LeftistHeap (Intersection Sphere Int)
        xs = intersections H.empty [i1, i2]
    hit xs `shouldBe` Just i2

  it "Hit when all intersections have negative t" $ do
    let s = Object 0 Sphere
        i1 = Intersection s (-1)
        i2 = Intersection s (-2)
        xs :: H.LeftistHeap (Intersection Sphere Int)
        xs = intersections H.empty [i1, i2]
    hit xs `shouldBe` Nothing

  prop "Hit is always the lowest non-negative intersections" $ \(ts :: [Int]) -> do
      let s = Object 0 Sphere
          xs :: H.LeftistHeap (Intersection Sphere Int)
          xs = intersections H.empty (map (Intersection s) ts)
      t <$> hit xs `shouldBe` if any (>= 0) ts
                                then Just (minimum (filter (>= 0) ts))
                                else Nothing
