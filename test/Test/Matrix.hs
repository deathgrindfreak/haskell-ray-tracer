{-# LANGUAGE OverloadedStrings #-}

module Test.Matrix
  ( spec_Matrix
  , test_Matrix
  )
where

import qualified Data.Vector              as V

import Hedgehog ((===), Gen)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR

import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Checkers hiding (inverse)
import           Test.QuickCheck.Classes

import           Test.Helper.Approximate
import           RayTracer.Matrix
import           RayTracer.Tuple
import           Test.Hspec
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

genSquareMatrix :: Int -> Gen (Matrix Double)
genSquareMatrix n = do
  es <- V.fromList <$> HG.list (HR.singleton (n * n)) (HG.int (HR.linear (-1000) 1000))
  return M { rows = n, cols = n, elements = V.map fromIntegral es }

test_Matrix :: Tasty.TestTree
test_Matrix = do
  Tasty.testGroup
    "Matrix"
    [ THH.testProperty "Inverse of Inverse should be original matrix" $
        HH.property $ do
          m <- HH.forAll $ genSquareMatrix 4

          HH.classify "non-invertable" (not . isInvertable $ m)

          if isInvertable m
            then (inverse (inverse m)) ~== m
            else safeInverse m === Nothing

    , THH.testProperty "Multiplication by another matrix and inverse should just be the original matrix" $
        HH.property $ do
          a <- HH.forAll $ genSquareMatrix 4
          b <- HH.forAll $ genSquareMatrix 4

          HH.classify "non-invertable" (not . isInvertable $ b)

          if isInvertable b
            then a * b * inverse b ~== a
            else safeInverse b === Nothing
    ]

spec_Matrix :: Spec
spec_Matrix = describe "Matrix" $ do
  it "Construction" $ do
    let m = fromLists [ [1, 2, 3, 4]
                      , [5.5, 6.5, 7.5, 8.5]
                      , [9, 10, 11, 12]
                      , [13.5, 14.5, 15.5, 16.5]
                      ]
        twos = fromList 2 2 [-3, 5, 1, -2]
        threes = fromList 3 3 [-3, 5, 0, 1, -2, -7, 0, 1, 1]

    m ! (0, 0) `shouldBe` 1
    m ! (0, 3) `shouldBe` 4
    m ! (1, 0) `shouldBe` 5.5
    m ! (1, 2) `shouldBe` 7.5
    m ! (2, 2) `shouldBe` 11
    m ! (3, 0) `shouldBe` 13.5
    m ! (3, 2) `shouldBe` 15.5

    twos ! (0, 0) `shouldBe` -3
    twos ! (0, 1) `shouldBe` 5
    twos ! (1, 0) `shouldBe` 1
    twos ! (1, 1) `shouldBe` -2

    threes ! (0, 0) `shouldBe` -3
    threes ! (1, 1) `shouldBe` -2
    threes ! (2, 2) `shouldBe` 1

  it "Equality" $ do
    let a = fromLists [ [1, 2, 3, 4]
                      , [5, 6, 7, 8]
                      , [9, 8, 7, 6]
                      , [5, 4, 3, 2]
                      ]
        b = fromLists [ [1, 2, 3, 4]
                      , [5, 6, 7, 8]
                      , [9, 8, 7, 6]
                      , [5, 4, 3, 2]
                      ]
    a == b `shouldBe` True
    a == ((+ 1) <$> b) `shouldBe` False

  it "Multiplication" $ do
    let a :: Matrix Int
        a = fromLists [ [1, 2, 3, 4]
                      , [5, 6, 7, 8]
                      , [9, 8, 7, 6]
                      , [5, 4, 3, 2]
                      ]
        b :: Matrix Int
        b = fromLists [ [-2, 1, 2, 3]
                      , [3, 2, 1, -1]
                      , [4, 3, 6, 5]
                      , [1, 2, 7, 8]
                      ]

        c :: Matrix Int
        c = fromLists [ [1, 2, 3, 4]
                      , [5, 6, 7, 8]
                      , [9, 8, 7, 6]
                      ]

        d :: Matrix Int
        d = fromLists [ [-2, 1, 2]
                      , [3, 2, 1]
                      , [4, 3, 6]
                      , [1, 2, 7]
                      ]

        e :: Matrix Int
        e = fromLists [ [1, 2, 3, 4, 5]
                      , [5, 6, 7, 8, 9]
                      , [9, 8, 7, 6, 7]
                      ]

        f :: Matrix Int
        f = fromLists [ [-2, 1, 2]
                      , [3, 2, 1]
                      , [4, 3, 6]
                      , [1, 2, 7]
                      , [8, 4, 2]
                      ]

    a * b `shouldBe` fromLists [ [20, 22, 50, 48]
                               , [44, 54, 114, 108]
                               , [40, 58, 110, 102]
                               , [16, 26, 46, 42]
                               ]

    c * d `shouldBe` fromLists [ [20, 22, 50]
                               , [44, 54, 114]
                               , [40, 58, 110]
                               ]

    f * e `shouldBe` fromLists [ [21, 18, 15, 12, 13]
                               , [22, 26, 30, 34, 40]
                               , [73, 74, 75, 76, 89]
                               , [74, 70, 66, 62, 72]
                               , [46, 56, 66, 76, 90]
                               ]

  prop "Transformation on point is commutative" $
    \(g :: Transform Double, p :: Point Double) -> do
      p |*| g `shouldBe` g |*| p

  it "Identity" $ do
    let a :: Matrix Int
        a = fromLists [ [1, 2, 3, 4]
                      , [5, 6, 7, 8]
                      , [9, 8, 7, 6]
                      , [5, 4, 3, 2]
                      ]

        b :: Matrix Double
        b = fromLists [ [-2, 1, 2, 3]
                      , [3, 2, 1, -1]
                      , [4, 3, 6, 5]
                      , [1, 2, 7, 8]
                      ]

    a * identity (rows a) `shouldBe` a
    b * identity (rows b) `shouldBe` b

  it "Transpose" $ do
    let a = fromLists [ [0, 9, 3, 0]
                      , [9, 8, 0, 8]
                      , [1, 8, 5, 3]
                      , [0, 0, 5, 8]
                      ]

    transpose a `shouldBe` fromLists [ [0, 9, 1, 0]
                                     , [9, 8, 8, 0]
                                     , [3, 0, 5, 5]
                                     , [0, 8, 3, 8]
                                     ]
    identity 5 `shouldBe` transpose (identity 5)

  it "Sub-Matrix" $ do
    let a = fromLists [ [1, 5, 0]
                      , [-3, 2, 7]
                      , [0, 6, -3]
                      ]
        b = fromLists [ [-6, 1, 1, 6]
                      , [-8, 5, 8, 6]
                      , [-1, 0, 8, 2]
                      , [-7, 1, -1, 1]
                      ]

    submatrix a 0 2 `shouldBe` fromList 2 2 [-3, 2, 0, 6]
    submatrix b 2 1 `shouldBe` fromLists [ [-6, 1, 6]
                                         , [-8, 8, 6]
                                         , [-7, -1, 1]
                                         ]

  it "Minor" $ do
    let a = fromLists [ [3, 5, 0]
                      , [2, -1, -7]
                      , [6, -1, 5]
                      ]
        b = submatrix a 1 0

    determinant b `shouldBe` 25
    minor a 1 0 `shouldBe` 25

  it "Cofactor" $ do
    let a = fromLists [ [3, 5, 0]
                      , [2, -1, -7]
                      , [6, -1, 5]
                      ]

    minor a 0 0 `shouldBe` -12
    cofactor a 0 0 `shouldBe` -12
    minor a 1 0 `shouldBe` 25
    cofactor a 1 0 `shouldBe` -25

  it "Determinant" $ do
    let a = fromList 2 2 [1, 5, -3, 2]
        b = fromLists [ [1, 2, 6]
                      , [-5, 8, -4]
                      , [2, 6, 4]
                      ]

        c = fromLists [ [-2, -8, 3, 5]
                      , [-3, 1, 7, 3]
                      , [1, 2, -9, 6]
                      , [-6, 7, 7, -9]
                      ]

    determinant a `shouldBe` 17

    cofactor b 0 0 `shouldBe` 56
    cofactor b 0 1 `shouldBe` 12
    cofactor b 0 2 `shouldBe` -46
    determinant b `shouldBe` -196

    cofactor c 0 0 `shouldBe` 690
    cofactor c 0 1 `shouldBe` 447
    cofactor c 0 2 `shouldBe` 210
    cofactor c 0 3 `shouldBe` 51
    determinant c `shouldBe` -4071

  it "Inverse" $ do
    let a = fromLists [ [6, 4, 4, 4]
                      , [5, 5, 7, 6]
                      , [4, -9, 3, -7]
                      , [9, 1, 7, -6]
                      ]
        b = fromLists [ [-4, 2, -2, -3]
                      , [9, 6, 2, 6]
                      , [0, -5, 1, -5]
                      , [0, 0, 0, 0]
                      ]

    determinant a `shouldBe` -2120
    isInvertable a `shouldBe` True

    determinant b `shouldBe` 0
    isInvertable b `shouldBe` False

  it "Translation" $ do
    let transform = translation 5 (-3) 2
        inv = inverse transform
        p = Point (-3) 4 5
        v = Vec (-3) 4 5
    transform |*| p `shouldBe` Point 2 1 7
    inv |*| p `shouldBe` Point (-8) 7 3
    transform |*| v `shouldBe` v

  prop "Translation via matrix should move point" $
    \(x :: Double, y :: Double, z :: Double, Point a b c :: Point Double) ->
      translation x y z |*| Point a b c `shouldBe` Point (a + x) (b + y) (c + z)

  it "Scaling" $ do
    let transform = scaling 2 3 4
        inv = inverse transform
        p = Point (-4) 6 8
        v = Vec (-4) 6 8
    transform |*| p `shouldBe` Point (-8) 18 32
    transform |*| v `shouldBe` Vec (-8) 18 32
    inv |*| v `shouldBe` Vec (-2) 2 2

  prop "Scaling via matrix should multiply point" $
    \(x :: Double, y :: Double, z :: Double, Point a b c :: Point Double) ->
      scaling x y z |*| Point a b c `shouldBe` Point (a * x) (b * y) (c * z)

  it "Reflection" $ do
    let transform = scaling (-1) 1 1
        p = Point 2 3 4
    transform |*| p `shouldBe` Point (-2) 3 4

  it "Rotation X" $ do
    let p :: Point Double
        p = Point 0 1 0
        halfQuarter :: Transform Double
        halfQuarter = rotationX (pi / 4)
        invHalfQuarter = inverse halfQuarter
        fullQuarter = rotationX (pi / 2)
    halfQuarter |*| p `shouldApproximate` Point 0 (sqrt 2 / 2) (sqrt 2 / 2)
    fullQuarter |*| p `shouldApproximate` Point 0 0 1
    invHalfQuarter |*| p `shouldApproximate` Point 0 (sqrt 2 / 2) (-sqrt 2 / 2)

  it "Rotation Y" $ do
    let p = Point 0 0 1
        halfQuarter = rotationY (pi / 4)
        fullQuarter = rotationY (pi / 2)
    halfQuarter |*| p `shouldApproximate` Point (sqrt 2 / 2) 0 (sqrt 2 / 2)
    fullQuarter |*| p `shouldApproximate` Point 1 0 0

  it "Rotation Z" $ do
    let p = Point 0 1 0
        halfQuarter = rotationZ (pi / 4)
        fullQuarter = rotationZ (pi / 2)
    halfQuarter |*| p `shouldApproximate` Point (-sqrt 2 / 2) (sqrt 2 / 2) 0
    fullQuarter |*| p `shouldApproximate` Point (-1) 0 0

  it "Shearing" $ do
    let p = Point 2 3 4
    shearing 1 0 0 0 0 0 |*| p `shouldBe` Point 5 3 4
    shearing 0 1 0 0 0 0 |*| p `shouldBe` Point 6 3 4
    shearing 0 0 1 0 0 0 |*| p `shouldBe` Point 2 5 4
    shearing 0 0 0 1 0 0 |*| p `shouldBe` Point 2 7 4
    shearing 0 0 0 0 1 0 |*| p `shouldBe` Point 2 3 6
    shearing 0 0 0 0 0 1 |*| p `shouldBe` Point 2 3 7

  it "Chaining" $ do
    let p :: Point Double
        p = Point 1 0 1
        a = rotationX (pi / 2)
        b = scaling 5 5 5
        c = translation 10 5 7
        p2 = a |*| p
        p3 = b |*| p2
        p4 = c |*| p3
    p2 `shouldApproximate` Point 1 (-1) 0
    p3 `shouldApproximate` Point 5 (-5) 0
    p4 `shouldApproximate` Point 15 0 7
    (c |*| b |*| a) |*| p `shouldBe` Point 15 0 7
    a |> b |> c |*| p `shouldBe` Point 15 0 7

  it "Functor" $ do
    let matrixTrigger :: Matrix (Double, Double, Double)
        matrixTrigger = undefined
    quickBatch (functor matrixTrigger)
