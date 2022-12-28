module MatrixSpec (spec) where

import Test.QuickCheck hiding (elements, inverse)
import Test.QuickCheck.Checkers hiding (inverse)
import Test.QuickCheck.Classes
import Test.Hspec.QuickCheck
import qualified Data.Vector as V
import Data.Functor ((<&>))

import SpecHelper
import RayTracer.Matrix
import RayTracer.Tuple

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    rows <- arbitrary
    cols <- arbitrary
    elements <- V.fromList <$> mapM (const arbitrary) [0..rows * cols]
    return $ M { rows, cols, elements }

instance Eq a => EqProp (Matrix a) where
  (=-=) = eq

newtype Square = Square { toMatrix :: Matrix Double }
  deriving (Show)

instance Arbitrary Square where
  arbitrary = do
    let n = 4
    elements :: V.Vector Int <- V.fromList <$> mapM (const arbitrary) [0..n * n]
    return . Square $ M { rows = n, cols = n, elements = V.map fromIntegral elements }

spec :: Spec
spec = describe "Matrix" $ do
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

        g :: Matrix Float
        g = fromLists [ [1, 2, 3, 4]
                      , [2, 4, 4, 2]
                      , [8, 6, 4, 1]
                      , [0, 0, 0, 1]
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

    g |*> Point 1 2 3 `shouldBe` Point 18 24 33
    Point 1 2 3 <*| transpose g `shouldBe` Point 18 24 33

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

  prop "Inverse of an Inverse should produce original matrix" $ \(Square m) ->
    if isInvertable m
      then (inverse m >>= inverse <&> fmap (fromIntegral . round)) `shouldBe` Just m
      else inverse m `shouldBe` Nothing

  prop "Multiplication by another matrix and inverse should just be the original matrix" $
    \(Square a, Square b) ->
      if isInvertable b
        then (inverse b >>= \b' -> return (a * b * b') <&> fmap (fromIntegral . round)) `shouldBe` Just a
        else inverse b `shouldBe` Nothing

  it "Functor" $ do
    let matrixTrigger :: Matrix (Double, Double, Double)
        matrixTrigger = undefined
    quickBatch (functor matrixTrigger)

-- propertyMatrixInverse (Square m) =
--   if isInvertable m
--     then inverse m `shouldBe` Nothing
--     else (inverse m >>= (round <$>) . inverse) `shouldBe` Just m
