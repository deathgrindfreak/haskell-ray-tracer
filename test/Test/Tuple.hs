module Test.Tuple (spec_Tuple) where

import           Test.Helper.Approximate
import           RayTracer.Tuple
import           Test.Hspec

spec_Tuple :: Spec
spec_Tuple = describe "Tuple" $ do
  it "Addition" $ do
    Point 3 (-2) 5 |+| Vec (-2) 3 1 `shouldBe` Point 1 1 6

  it "Subtraction" $ do
    Vec 3 2 1 |-| Vec 5 6 7 `shouldBe` Vec (-2) (-4) (-6)
    Point 3 2 1 |-| Point 5 6 7 `shouldBe` Vec (-2) (-4) (-6)

  it "Scalar Multiplication" $ do
    Scalar 3.5 |*| Vec 1 (-2) 3 `shouldBe` Vec 3.5 (-7) 10.5
    Scalar 0.5 |*| Vec 2 (-4) 6 `shouldBe` Vec 1 (-2) 3

  it "Magnitude" $ do
    magnitude (Vec 1 0 0) `shouldBe` 1
    magnitude (Vec 0 1 0) `shouldBe` 1
    magnitude (Vec 0 0 1) `shouldBe` 1
    magnitude (Vec 1 2 3) `shouldBe` sqrt 14
    magnitude (Vec (-1) (-2) (-3)) `shouldBe` sqrt 14

  it "norm" $ do
    norm (Vec 4 0 0) `shouldBe` Vec 1 0 0
    norm (Vec 1 2 3) `shouldBe` Vec (1 / sqrt 14) (2 / sqrt 14) (3 / sqrt 14)
    magnitude (norm (Vec 1 2 3)) `shouldBe` 1

  it "dot" $ do
    Vec 1 2 3 |*| Vec 2 3 4 `shouldBe` Scalar 20

  it "cross" $ do
    let a = Vec 1 2 3
    let b = Vec 2 3 4
    a `cross` b `shouldBe` Vec (-1) 2 (-1)
    b `cross` a `shouldBe` Vec 1 (-2) 1

  it "Reflect a vector approaching 45 degrees" $ do
    let v = Vec 1 (-1) 0
        n = Vec 0 1 0
    reflect v n `shouldBe` Vec 1 1 0

  it "Reflect a vector off a slanted surface" $ do
    let v = Vec 0 (-1) 0
        n = Vec (sqrt 2 / 2) (sqrt 2 / 2) 0
    reflect v n `shouldApproximate` Vec 1 0 0
