module Test.Ray (spec_Ray) where

import RayTracer.Light hiding (position)
import RayTracer.Ray
import RayTracer.Transform
import RayTracer.Tuple
import Test.Helper.Approximate
import Test.Helper.Util
import Test.Hspec

import Control.Lens hiding ((|>))
import Data.Maybe (isNothing)
import Test.Hspec.QuickCheck
import Test.QuickCheck (classify)

spec_Ray :: Spec
spec_Ray = describe "Ray" $ do
  it "Position" $ do
    let r = Ray (Point 2 3 4) (Vec 1 0 0)
    position r 0 `shouldBe` Point 2 3 4
    position r 1 `shouldBe` Point 3 3 4
    position r (-1) `shouldBe` Point 1 3 4
    position r 2.5 `shouldBe` Point 4.5 3 4

  it "Intersect ray through sphere" $ do
    let r :: Ray Double
        r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
        s = makeSphere 0
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [4.0, 6.0]

  it "Intersect ray tangent to sphere" $ do
    let r = Ray (Point 0 1 (-5)) (Vec 0 0 1)
        s = makeSphere 0
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [5.0, 5.0]

  it "Intersect ray misses sphere" $ do
    let r = Ray (Point 0 2 (-5)) (Vec 0 0 1)
        s = makeSphere 0
        xs = s `intersect` r
    xs `shouldBe` []

  it "Intersect ray inside of sphere" $ do
    let r = Ray (Point 0 0 0) (Vec 0 0 1)
        s = makeSphere 0
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [-1.0, 1.0]

  it "Intersect ray ahead of sphere" $ do
    let r = Ray (Point 0 0 5) (Vec 0 0 1)
        s = makeSphere 0
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [-6.0, -4.0]

  it "Hit all intersections have positive t" $ do
    let s = makeSphere 0
        i1 = Intersection s 1
        i2 = Intersection s 2
        xs = intersections [i1, i2]
    hit xs `shouldBe` Just i1

  it "Hit when some intersections have negative t" $ do
    let s = makeSphere 0
        i1 = Intersection s (-1)
        i2 = Intersection s 2
        xs = intersections [i1, i2]
    hit xs `shouldBe` Just i2

  it "Hit when all intersections have negative t" $ do
    let s = makeSphere 0
        i1 = Intersection s (-1)
        i2 = Intersection s (-2)
        xs = intersections [i1, i2]
    hit xs `shouldBe` Nothing

  prop "Hit is always the lowest non-negative intersections" $ \(ts :: [Double]) -> do
    let s = makeSphere 0
        xs = intersections (map (Intersection s) ts)
        ints =
          if any (>= 0) ts
            then Just (minimum (filter (>= 0) ts))
            else Nothing
    classify (isNothing ints) "No intersections" $
      (^. t) <$> hit xs `shouldBe` ints

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
        s =
          Object
            { _objectId = 0
            , _shape = Sphere
            , _transform = scaling 2 2 2
            , _material = defaultMaterial
            }
        xs = s `intersect` r
    xs `shouldBe` map (Intersection s) [3, 7]

  it "Intersect a scaled shape with a ray" $ do
    let r :: Ray Double
        r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
        s =
          Object
            { _objectId = 0
            , _shape = Sphere
            , _transform = scaling 2 2 2
            , _material = defaultMaterial
            }
    localRay s r `shouldBe` Ray (Point 0 0 (-2.5)) (Vec 0 0 0.5)

  it "Intersect a translated sphere with a ray" $ do
    let r :: Ray Double
        r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
        s =
          Object
            { _objectId = 0
            , _shape = Sphere
            , _transform = translation 5 0 0
            , _material = defaultMaterial
            }
        xs = s `intersect` r
    xs `shouldBe` []

  it "Intersect a translated shape with a ray" $ do
    let r :: Ray Double
        r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
        s =
          Object
            { _objectId = 0
            , _shape = Sphere
            , _transform = translation 5 0 0
            , _material = defaultMaterial
            }
    localRay s r `shouldBe` Ray (Point (-5) 0 (-5)) (Vec 0 0 1)

  it "Normal on a sphere with point on x axis" $ do
    let s = makeSphere 0
    normalAt s (Point 1 0 0) `shouldBe` Vec 1 0 0

  it "Normal on a sphere with point on y axis" $ do
    let s = makeSphere 0
    normalAt s (Point 0 1 0) `shouldBe` Vec 0 1 0

  it "Normal on a sphere with point on z axis" $ do
    let s = makeSphere 0
    normalAt s (Point 0 0 1) `shouldBe` Vec 0 0 1

  it "Normal on a sphere on non-axial point" $ do
    let s = makeSphere 0
    normalAt s (Point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)) `shouldBe` Vec (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)

  it "Computing the normal on a translated sphere" $ do
    let s = Object 0 Sphere (translation 0 1 0) defaultMaterial
        n = normalAt s (Point 0 1.70711 (-0.70711))
    n `shouldApproximate` Vec 0 0.70711 (-0.70711)

  it "Computing the normal on a translated shape" $ do
    let s = Object 0 Sphere (translation 0 1 0) defaultMaterial
        n = testNormalAt s (Point 0 1.70711 (-0.70711))
    n `shouldApproximate` Vec 0 0.70711 (-0.70711)

  it "Computing the normal on a transformed sphere" $ do
    let s = Object 0 Sphere (rotationZ (pi / 5) |> scaling 1 0.5 1) defaultMaterial
        n = normalAt s (Point 0 (sqrt 2 / 2) (-sqrt 2 / 2))
    n `shouldApproximate` Vec 0 0.97014 (-0.24254)

  it "Computing the normal on a transformed shape" $ do
    let s = Object 0 Sphere (rotationZ (pi / 5) |> scaling 1 0.5 1) defaultMaterial
        n = testNormalAt s (Point 0 (sqrt 2 / 2) (-sqrt 2 / 2))
    n `shouldApproximate` Vec 0 0.97014 (-0.24254)

  it "The normal of a plane is constant everywhere" $ do
    let p = defaultShape Plane & objectId .~ 0
        n1 = localNormalAt p (Point 0 0 0)
        n2 = localNormalAt p (Point 10 0 (-10))
        n3 = localNormalAt p (Point (-5) 0 150)
    n1 `shouldBe` Vec 0 1 0
    n2 `shouldBe` Vec 0 1 0
    n3 `shouldBe` Vec 0 1 0

testNormalAt :: Object HasId -> Point Double -> Vec Double
testNormalAt s p = normalAtFor s p (\(Point x y z) -> Vec x y z)
