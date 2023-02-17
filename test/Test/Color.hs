module Test.Color (spec_Color) where

import RayTracer.Color
import Test.Hspec

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

spec_Color :: Spec
spec_Color = describe "Color" $ do
  it "Addition" $ do
    Color 9 6 75 + Color 7 1 25 `shouldBe` Color 16 7 100
  it "Subtraction" $ do
    Color 9 6 75 - Color 7 1 25 `shouldBe` Color 2 5 50
  it "Multiplication" $ do
    Color 2 3 4 * Color 1 2 3 `shouldBe` Color 2 6 12
    2 * Color 1 2 3 `shouldBe` Color 2 4 6
  it "Functor" $ do
    let colorTrigger :: Color (Double, Double, Double)
        colorTrigger = undefined
    quickBatch (functor colorTrigger)
  it "Applicative" $ do
    let colorTrigger :: Color (Double, Double, Double)
        colorTrigger = undefined
    quickBatch (applicative colorTrigger)
