module RaySpec (spec) where

import Test.Hspec.QuickCheck
import SpecHelper

import RayTracer.Ray
import RayTracer.Tuple
import RayTracer.Matrix
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
        s = Object 0 makeSphere
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [4.0, 6.0]

  it "Intersect ray tangent to sphere" $ do
    let r = Ray (Point 0 1 (-5)) (Vec 0 0 1)
        s = Object 0 makeSphere
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [5.0, 5.0]

  it "Intersect ray misses sphere" $ do
    let r = Ray (Point 0 2 (-5)) (Vec 0 0 1)
        s = Object 0 makeSphere
        xs = s `intersect` r
    xs `shouldBe` []

  it "Intersect ray inside of sphere" $ do
    let r = Ray (Point 0 0 0) (Vec 0 0 1)
        s = Object 0 makeSphere
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [-1.0, 1.0]

  it "Intersect ray ahead of sphere" $ do
    let r = Ray (Point 0 0 5) (Vec 0 0 1)
        s = Object 0 makeSphere
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [-6.0, -4.0]

  it "Hit all intersections have positive t" $ do
    let s = Object 0 makeSphere
        i1 = Intersection s 1
        i2 = Intersection s 2
        xs :: H.LeftistHeap (Intersection (Sphere Double) Int)
        xs = intersections H.empty [i1, i2]
    hit xs `shouldBe` Just i1

  it "Hit when some intersections have negative t" $ do
    let s = Object 0 makeSphere
        i1 = Intersection s (-1)
        i2 = Intersection s 2
        xs :: H.LeftistHeap (Intersection (Sphere Double) Int)
        xs = intersections H.empty [i1, i2]
    hit xs `shouldBe` Just i2

  it "Hit when all intersections have negative t" $ do
    let s = Object 0 makeSphere
        i1 = Intersection s (-1)
        i2 = Intersection s (-2)
        xs :: H.LeftistHeap (Intersection (Sphere Double) Int)
        xs = intersections H.empty [i1, i2]
    hit xs `shouldBe` Nothing

  prop "Hit is always the lowest non-negative intersections" $ \(ts :: [Int]) -> do
      let s = Object 0 makeSphere
          xs :: H.LeftistHeap (Intersection (Sphere Double) Int)
          xs = intersections H.empty (map (Intersection s) ts)
      t <$> hit xs `shouldBe` if any (>= 0) ts
                                then Just (minimum (filter (>= 0) ts))
                                else Nothing

  it "Translating a ray" $ do
    let r :: Ray Double
        r = Ray (Point 1 2 3) (Vec 0 1 0)
    r |*| translation 3 4 5 `shouldBe` Ray (Point 4 6 8) (Vec 0 1 0)
    translation 3 4 5 |*| r `shouldBe` Ray (Point 4 6 8) (Vec 0 1 0)

  it "Scaling a ray" $ do
    let r :: Ray Double
        r = Ray (Point 1 2 3) (Vec 0 1 0)
    r |*| scaling 2 3 4 `shouldBe` Ray (Point 2 6 12) (Vec 0 3 0)
    scaling 2 3 4 |*| r `shouldBe` Ray (Point 2 6 12) (Vec 0 3 0)

  it "Intersect a scaled sphere with a ray" $ do
    let r :: Ray Double
        r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
        s = Object 0 Sphere { transform = scaling 2 2 2 }
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [3, 7]

  it "Intersect a translated sphere with a ray" $ do
    let r :: Ray Double
        r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
        s = Object 0 Sphere { transform = translation 5 0 0 }
        xs = s `intersect` r
    xs `shouldBe` []
