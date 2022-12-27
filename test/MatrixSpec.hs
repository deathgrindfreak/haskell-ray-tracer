module MatrixSpec (spec) where

import SpecHelper
import RayTracer.Matrix

spec :: Spec
spec = describe "Matrix" $ do
  it "Construction" $ do
    let m = fromLists [ [1, 2, 3, 4]
                      , [5.5, 6.5, 7.5, 8.5]
                      , [9, 10, 11, 12]
                      , [13.5, 14.5, 15.5, 16.5]
                      ]
    m ! (0, 0) `shouldBe` 1
    m ! (0, 3) `shouldBe` 4
    m ! (1, 0) `shouldBe` 5.5
    m ! (1, 2) `shouldBe` 7.5
    m ! (2, 2) `shouldBe` 11
    m ! (3, 0) `shouldBe` 13.5
    m ! (3, 2) `shouldBe` 15.5
