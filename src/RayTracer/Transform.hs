{-# LANGUAGE LambdaCase #-}

module RayTracer.Transform
  ( Transform
  , identityTransform
  , translation
  , scaling
  , rotationX
  , rotationY
  , rotationZ
  , shearing
  , (|>)
  )
where

import Test.QuickCheck hiding (elements)

import RayTracer.Matrix
import RayTracer.Tuple

newtype Transform a
  = Transform (Matrix a)

instance MatrixLike Transform where
  isInvertable _ = True
  inverse (Transform m) = Transform $ inverse m
  transpose (Transform m) = Transform $ transpose m

instance (Num a, Ord a, Show a) => Show (Transform a) where
  show (Transform m) = prettyPrintMatrix m

instance Eq a => Eq (Transform a) where
  Transform m == Transform m' = m == m'

instance VecMult Transform Vec Vec where
  Transform m |*| t = toVecCol (m * fromVecCol t)

instance VecMult Vec Transform Vec where
  t |*| Transform m = toVecRow (fromVecRow t * transpose m)

instance VecMult Transform Point Point where
  Transform m |*| t = toPointCol (m * fromPointCol t)

instance VecMult Point Transform Point where
  t |*| Transform m = toPointRow (fromPointRow t * transpose m)

instance VecMult Transform Transform Transform where
  Transform a |*| Transform b = Transform (a * b)

identityTransform :: Num a => Transform a
identityTransform = Transform $ identity 4

translation :: Num a => a -> a -> a -> Transform a
translation x y z =
  Transform $
    fromLists
      [ [1, 0, 0, x]
      , [0, 1, 0, y]
      , [0, 0, 1, z]
      , [0, 0, 0, 1]
      ]

scaling :: Num a => a -> a -> a -> Transform a
scaling x y z =
  Transform $
    fromLists
      [ [x, 0, 0, 0]
      , [0, y, 0, 0]
      , [0, 0, z, 0]
      , [0, 0, 0, 1]
      ]

rotationX :: Floating a => a -> Transform a
rotationX r =
  Transform $
    fromLists
      [ [1, 0, 0, 0]
      , [0, cos r, -sin r, 0]
      , [0, sin r, cos r, 0]
      , [0, 0, 0, 1]
      ]

rotationY :: Floating a => a -> Transform a
rotationY r =
  Transform $
    fromLists
      [ [cos r, 0, sin r, 0]
      , [0, 1, 0, 0]
      , [-sin r, 0, cos r, 0]
      , [0, 0, 0, 1]
      ]

rotationZ :: Floating a => a -> Transform a
rotationZ r =
  Transform $
    fromLists
      [ [cos r, -sin r, 0, 0]
      , [sin r, cos r, 0, 0]
      , [0, 0, 1, 0]
      , [0, 0, 0, 1]
      ]

shearing :: Num a => a -> a -> a -> a -> a -> a -> Transform a
shearing xy xz yx yz zx zy =
  Transform $
    fromLists
      [ [1, xy, xz, 0]
      , [yx, 1, yz, 0]
      , [zx, zy, 1, 0]
      , [0, 0, 0, 1]
      ]

(|>) :: Num a => Transform a -> Transform a -> Transform a
Transform a |> Transform b = Transform (b * a)

fromVecCol :: Num a => Vec a -> Matrix a
fromVecCol (Vec a b c) = fromList 4 1 [a, b, c, 0]

fromPointCol :: Num a => Point a -> Matrix a
fromPointCol (Point a b c) = fromList 4 1 [a, b, c, 1]

fromVecRow :: Num a => Vec a -> Matrix a
fromVecRow (Vec a b c) = fromList 1 4 [a, b, c, 0]

fromPointRow :: Num a => Point a -> Matrix a
fromPointRow (Point a b c) = fromList 1 4 [a, b, c, 1]

toVecCol :: Matrix a -> Vec a
toVecCol m = Vec (m ! (0, 0)) (m ! (1, 0)) (m ! (2, 0))

toPointCol :: Matrix a -> Point a
toPointCol m = Point (m ! (0, 0)) (m ! (1, 0)) (m ! (2, 0))

toVecRow :: Matrix a -> Vec a
toVecRow m' = Vec (m' ! (0, 0)) (m' ! (0, 1)) (m' ! (0, 2))

toPointRow :: Matrix a -> Point a
toPointRow m' = Point (m' ! (0, 0)) (m' ! (0, 1)) (m' ! (0, 2))

instance (Arbitrary a, Num a) => Arbitrary (Transform a) where
  arbitrary =
    Transform
      <$> matrixM
        4
        4
        ( \case
            (3, 3) -> return 1
            (3, _) -> return 0
            _ -> arbitrary
        )
