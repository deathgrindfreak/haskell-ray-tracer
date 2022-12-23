module TupleSpec (spec) where

import SpecHelper
import Lib

spec :: Spec
spec = describe "Tuple" $ do
  it "Addition" $ do
    Point 3 (-2) 5 + Vec (-2) 3 1 `shouldBe` Point 1 1 6
  it "Subtraction" $ do
    Vec 3 2 1 - Vec 5 6 7 `shouldBe` Vec (-2) (-4) (-6)
    Point 3 2 1 - Point 5 6 7 `shouldBe` Vec (-2) (-4) (-6)
  it "Scalar Multiplication" $ do
    Scalar 3.5 * Vec 1 (-2) 3 `shouldBe` Vec 3.5 (-7) 10.5
    Scalar 0.5 * Vec 2 (-4) 6 `shouldBe` Vec 1 (-2) 3
  it "Magnitude" $ do
    abs (Vec 1 0 0) `shouldBe` Scalar 1
    abs (Vec 0 1 0) `shouldBe` Scalar 1
    abs (Vec 0 0 1) `shouldBe` Scalar 1
    abs (Vec 1 2 3) `shouldBe` Scalar (sqrt 14)
    abs (Vec (-1) (-2) (-3)) `shouldBe` Scalar (sqrt 14)
  it "norm" $ do
    norm (Vec 4 0 0) `shouldBe` Vec 1 0 0
    norm (Vec 1 2 3) `shouldBe` Vec (1 / sqrt 14) (2 / sqrt 14) (3 / sqrt 14)
    abs (norm (Vec 1 2 3)) `shouldBe` 1
  it "dot" $ do
    Vec 1 2 3 * Vec 2 3 4 `shouldBe` 20
  it "cross" $ do
    let a = Vec 1 2 3
    let b = Vec 2 3 4
    a `cross` b `shouldBe` Vec (-1) 2 (-1)
    b `cross` a `shouldBe` Vec 1 (-2) 1
