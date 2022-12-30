{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module RayTracer.Matrix
  ( Matrix(..)
  , TransformMatrix
  , (!)
  , update
  , dims
  , (//)
  , elemAt
  , matrix
  , fromList
  , fromLists
  , toLists
  , elementwise
  , transpose
  , identity
  , submatrix
  , minor
  , cofactor
  , determinant
  , isInvertable
  , inverse
  , translation
  , scaling
  , rotationX
  , rotationY
  , rotationZ
  , shearing
  , (|>)
  )
where

import RayTracer.Tuple

import Test.QuickCheck hiding (elements)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Checkers (EqProp, eq, (=-=))

import qualified Data.Vector as V
import Data.List (intercalate)
import Data.Bifunctor (Bifunctor(first))

data Matrix a = M
  { rows :: !Int
  , cols :: !Int
  , elements :: V.Vector a
  }

dims :: Matrix a -> (Int, Int)
dims M { rows, cols } = (rows, cols)

instance (Num a, Ord a, Show a) => Show (Matrix a) where
  show = prettyPrintMatrix

instance Eq a => Eq (Matrix a) where
  m == m' =
    rows m == rows m' && cols m == cols m' && (V.and . elements $ elementwise (==) m m')

prettyPrintMatrix :: Show a => Matrix a -> String
prettyPrintMatrix m@M { rows, cols } =
  let ls = lengths m
      border = replicate (sum ls + 3 * cols - 1) '═'
      rowValues i = [padRight (show (m ! (i, j))) (ls !! j) | j <- [0..cols-1]]
  in unlines $
        ("╔" ++ border ++ "╗") :
        ["║ " ++ intercalate " ┃ " (rowValues i) ++ " ║" | i <- [0..rows-1]]
        ++ ["╚" ++ border ++ "╝"]
  where
    lengths = foldr1 (zipWith max) . map (map (length . show)) . toLists
    padRight str l = str ++ replicate (max 0 (l - length str)) ' '

instance Functor Matrix where
  fmap f m@M { elements } = m { elements = fmap f elements }

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix rows cols f =
  M rows cols . V.fromList $ [f (r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]

matrixM :: (Monad m) => Int -> Int -> ((Int, Int) -> m a) -> m (Matrix a)
matrixM rows cols f =
  M rows cols . V.fromList <$> sequence [f (r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]

elementwise :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
elementwise f (M ar ac ea) (M br bc eb) =
  if ar == br && ac == bc
    then M {rows = ar, cols = ac, elements = V.zipWith f ea eb}
    else error "Rows and columns must match"

instance Num a => Num (Matrix a) where
  negate = fmap negate
  signum = fmap signum
  abs = fmap abs
  (+) = elementwise (+)
  (-) = elementwise (-)
  m * m' =
    if cols m == rows m'
      then fromLists [[ multRowCol r c | c <- [0..cols m' - 1]] | r <- [0..rows m - 1]]
      else error "Improper dimensions for multiplication"
    where
      multRowCol r c = sum [m ! (r, i) * m' ! (i, c) | i <- [0..cols m - 1]]
  fromInteger = M 1 1 . V.singleton . fromInteger

idx :: Matrix a -> Int -> Int -> Int
idx M { cols } r c = c + r * cols

elemAt :: Matrix a -> Int -> Int -> a
elemAt m@M { elements } i j = elements V.! idx m i j

(!) :: Matrix a -> (Int, Int) -> a
(!) m (i, j) = elemAt m i j

update :: [((Int, Int), a)] -> Matrix a -> Matrix a
update vals m@M { elements } =
  m { elements = elements V.// map (first (uncurry (idx m))) vals }

(//) :: Matrix a -> [((Int, Int), a)] -> Matrix a
(//) = flip update

toLists :: Matrix a -> [[a]]
toLists m@M { rows, cols } = [[m ! (i, j) | j <- [0..cols-1]] | i <- [0..rows-1]]

fromList :: Int -> Int -> [a] -> Matrix a
fromList rows cols lst =
  let elements = take (cols * rows) lst
  in if cols * rows > length elements
       then error "List does not correspond to dimensions"
       else M { rows, cols, elements = V.fromList elements }

fromLists :: [[a]] -> Matrix a
fromLists [] = error "Empty list"
fromLists l@(xs:_) = fromList (length l) (length xs) (concat l)

identity :: Num a => Int -> Matrix a
identity n = matrix n n (\(i, j) -> if i == j then 1 else 0)

transpose :: Matrix a -> Matrix a
transpose m@M { rows = n } = matrix n n (\(i, j) -> m ! (j, i))

submatrix :: Matrix a -> Int -> Int -> Matrix a
submatrix m@M { rows, cols } r c =
  matrix (rows-1) (cols-1) (\(i, j) ->
                              let i' = if i < r then i else i + 1
                                  j' = if j < c then j else j + 1
                               in m ! (i', j'))

minor :: Num a => Matrix a -> Int -> Int -> a
minor m r c = determinant $ submatrix m r c

cofactor :: Num a => Matrix a -> Int -> Int -> a
cofactor m r c = (if even (r + c) then 1 else -1) * minor m r c

determinant :: Num a => Matrix a -> a
determinant m@M { rows, cols }
  | rows /= cols = error "Can only take determinant of square matrix"
  | rows == 2 = (m ! (0, 0)) * (m ! (1, 1)) - (m ! (1, 0)) * (m ! (0, 1))
  | otherwise = sum [(m ! (0, c)) * cofactor m 0 c | c <- [0..cols - 1]]

class Invertable m where
  isInvertable :: (Num a, Eq a) => m a -> Bool
  inverse :: (Fractional a, Eq a) => m a -> Maybe (m a)

instance Invertable Matrix where
  isInvertable m = determinant m /= 0
  inverse m
    | not $ isInvertable m = Nothing
    | otherwise =
        Just $ matrix (rows m) (cols m) (\(i, j) -> cofactor m j i / determinant m)

instance Invertable TransformMatrix where
  isInvertable _ = True
  inverse (TransformMatrix m) = TransformMatrix <$> inverse m

newtype TransformMatrix a = TransformMatrix (Matrix a)

instance (Num a, Ord a, Show a) => Show (TransformMatrix a) where
  show (TransformMatrix m) = prettyPrintMatrix m

instance Eq a => Eq (TransformMatrix a) where
  TransformMatrix m == TransformMatrix m' = m == m'

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

instance VecMult TransformMatrix Vec Vec where
  TransformMatrix m |*| t = toVecCol (m * fromVecCol t)

instance VecMult Vec TransformMatrix Vec where
  t |*| TransformMatrix m = toVecRow (fromVecRow t * transpose m)

instance VecMult TransformMatrix Point Point where
  TransformMatrix m |*| t = toPointCol (m * fromPointCol t)

instance VecMult Point TransformMatrix Point where
  t |*| TransformMatrix m = toPointRow (fromPointRow t * transpose m)

instance VecMult TransformMatrix TransformMatrix TransformMatrix where
  TransformMatrix a |*| TransformMatrix b = TransformMatrix (a * b)

translation :: Num a => a -> a -> a -> TransformMatrix a
translation x y z = TransformMatrix $
  fromLists [ [1, 0, 0, x]
            , [0, 1, 0, y]
            , [0, 0, 1, z]
            , [0, 0, 0, 1]
            ]

scaling :: Num a => a -> a -> a -> TransformMatrix a
scaling x y z = TransformMatrix $
  fromLists [ [x, 0, 0, 0]
            , [0, y, 0, 0]
            , [0, 0, z, 0]
            , [0, 0, 0, 1]
            ]

rotationX :: Floating a => a -> TransformMatrix a
rotationX r = TransformMatrix $
  fromLists [ [1, 0, 0, 0]
            , [0, cos r, -sin r, 0]
            , [0, sin r, cos r, 0]
            , [0, 0, 0, 1]
            ]

rotationY :: Floating a => a -> TransformMatrix a
rotationY r = TransformMatrix $
  fromLists [ [cos r, 0, sin r, 0]
            , [0, 1, 0, 0]
            , [-sin r, 0, cos r, 0]
            , [0, 0, 0, 1]
            ]

rotationZ :: Floating a => a -> TransformMatrix a
rotationZ r = TransformMatrix $
  fromLists [ [cos r, -sin r, 0, 0]
            , [sin r, cos r, 0, 0]
            , [0, 0, 1, 0]
            , [0, 0, 0, 1]
            ]

shearing :: Num a => a -> a -> a -> a -> a -> a -> TransformMatrix a
shearing xy xz yx yz zx zy = TransformMatrix $
  fromLists [ [1, xy, xz, 0]
            , [yx, 1, yz, 0]
            , [zx, zy, 1, 0]
            , [0, 0, 0, 1]
            ]

(|>) :: Num a => TransformMatrix a -> TransformMatrix a -> TransformMatrix a
TransformMatrix a |> TransformMatrix b = TransformMatrix (b * a)

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    rows <- Q.elements [2..10]
    cols <- Q.elements [2..10]
    matrixM rows cols (const arbitrary)

instance Eq a => EqProp (Matrix a) where
  (=-=) = eq

instance (Arbitrary a, Num a) => Arbitrary (TransformMatrix a) where
  arbitrary = TransformMatrix <$>
    matrixM 4 4 (\case
                    (3, 3) -> return 1
                    (3, _) -> return 0
                    _ -> arbitrary)

instance Eq a => EqProp (TransformMatrix a) where
  (=-=) = eq
