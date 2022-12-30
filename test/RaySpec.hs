module RaySpec (spec) where

import SpecHelper
import RayTracer.Ray
import RayTracer.Tuple

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
        s = Sphere 0
        xs = s `intersect` r
    4.0 `elem` xs `shouldBe` True
    6.0 `elem` xs `shouldBe` True

  it "Intersect ray tangent to sphere" $ do
    let r = Ray (Point 0 1 (-5)) (Vec 0 0 1)
        s = Sphere 0
        xs = s `intersect` r
    5.0 `elem` xs `shouldBe` True

  it "Intersect ray misses sphere" $ do
    let r = Ray (Point 0 2 (-5)) (Vec 0 0 1)
        s = Sphere 0
        xs = s `intersect` r
    length xs `shouldBe` 0
