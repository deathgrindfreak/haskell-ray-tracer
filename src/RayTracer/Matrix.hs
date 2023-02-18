{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module RayTracer.Matrix
  ( Matrix (..)
  , prettyPrintMatrix
  , (!)
  , update
  , dims
  , (//)
  , elemAt
  , matrix
  , matrixM
  , fromList
  , fromLists
  , toLists
  , elementwise
  , identity
  , submatrix
  , minor
  , cofactor
  , determinant
  , MatrixLike (..)
  )
where

import Test.QuickCheck hiding (elements)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Checkers (EqProp, eq, (=-=))

import Data.Bifunctor (Bifunctor (first))
import Data.List (intercalate)
import qualified Data.Vector as V

data Matrix a = M
  { rows :: !Int
  , cols :: !Int
  , elements :: V.Vector a
  }

dims :: Matrix a -> (Int, Int)
dims M {rows, cols} = (rows, cols)

instance (Num a, Ord a, Show a) => Show (Matrix a) where
  show = prettyPrintMatrix

instance Eq a => Eq (Matrix a) where
  m == m' =
    rows m == rows m' && cols m == cols m' && (V.and . elements $ elementwise (==) m m')

prettyPrintMatrix :: Show a => Matrix a -> String
prettyPrintMatrix m@M {rows, cols} =
  let ls = lengths m
      border = replicate (sum ls + 3 * cols - 1) '═'
      rowValues i = [padRight (show (m ! (i, j))) (ls !! j) | j <- [0 .. cols - 1]]
   in unlines $
        ("\n╔" ++ border ++ "╗")
          : ["║ " ++ intercalate " ┃ " (rowValues i) ++ " ║" | i <- [0 .. rows - 1]]
          ++ ["╚" ++ border ++ "╝"]
  where
    lengths = foldr1 (zipWith max) . map (map (length . show)) . toLists
    padRight str l = str ++ replicate (max 0 (l - length str)) ' '

instance Functor Matrix where
  fmap f m@M {elements} = m {elements = fmap f elements}

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix rows cols f =
  M rows cols . V.fromList $ [f (r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

matrixM :: (Monad m) => Int -> Int -> ((Int, Int) -> m a) -> m (Matrix a)
matrixM rows cols f =
  M rows cols . V.fromList <$> sequence [f (r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

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
      then fromLists [[multRowCol r c | c <- [0 .. cols m' - 1]] | r <- [0 .. rows m - 1]]
      else error "Improper dimensions for multiplication"
    where
      multRowCol r c = sum [m ! (r, i) * m' ! (i, c) | i <- [0 .. cols m - 1]]
  fromInteger = M 1 1 . V.singleton . fromInteger

idx :: Matrix a -> Int -> Int -> Int
idx M {cols} r c = c + r * cols

elemAt :: Matrix a -> Int -> Int -> a
elemAt m@M {elements} i j = elements V.! idx m i j

(!) :: Matrix a -> (Int, Int) -> a
(!) m (i, j) = elemAt m i j

update :: [((Int, Int), a)] -> Matrix a -> Matrix a
update vals m@M {elements} =
  m {elements = elements V.// map (first (uncurry (idx m))) vals}

(//) :: Matrix a -> [((Int, Int), a)] -> Matrix a
(//) = flip update

toLists :: Matrix a -> [[a]]
toLists m@M {rows, cols} = [[m ! (i, j) | j <- [0 .. cols - 1]] | i <- [0 .. rows - 1]]

fromList :: Int -> Int -> [a] -> Matrix a
fromList rows cols lst =
  let elements = take (cols * rows) lst
   in if cols * rows > length elements
        then error "List does not correspond to dimensions"
        else M {rows, cols, elements = V.fromList elements}

fromLists :: [[a]] -> Matrix a
fromLists [] = error "Empty list"
fromLists l@(xs : _) = fromList (length l) (length xs) (concat l)

identity :: Num a => Int -> Matrix a
identity n = matrix n n (\(i, j) -> if i == j then 1 else 0)

submatrix :: Matrix a -> Int -> Int -> Matrix a
submatrix m@M {rows, cols} r c =
  matrix
    (rows - 1)
    (cols - 1)
    ( \(i, j) ->
        let i' = if i < r then i else i + 1
            j' = if j < c then j else j + 1
         in m ! (i', j')
    )

minor :: Num a => Matrix a -> Int -> Int -> a
minor m r c = determinant $ submatrix m r c

cofactor :: Num a => Matrix a -> Int -> Int -> a
cofactor m r c = (if even (r + c) then 1 else -1) * minor m r c

determinant :: Num a => Matrix a -> a
determinant m@M {rows, cols}
  | rows /= cols = error "Can only take determinant of square matrix"
  | rows == 2 = (m ! (0, 0)) * (m ! (1, 1)) - (m ! (1, 0)) * (m ! (0, 1))
  | otherwise = sum [(m ! (0, c)) * cofactor m 0 c | c <- [0 .. cols - 1]]

class MatrixLike m where
  {-# MINIMAL isInvertable, inverse, transpose #-}

  isInvertable :: (Num a, Eq a) => m a -> Bool
  inverse :: (Fractional a, Eq a) => m a -> m a
  transpose :: m a -> m a

  safeInverse :: (Fractional a, Eq a) => m a -> Maybe (m a)
  safeInverse m
    | not $ isInvertable m = Nothing
    | otherwise = Just $ inverse m

instance MatrixLike Matrix where
  isInvertable m = determinant m /= 0
  inverse m = matrix (rows m) (cols m) (\(i, j) -> cofactor m j i / determinant m)
  transpose m@M {rows = n} = matrix n n (\(i, j) -> m ! (j, i))

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    rows <- Q.elements [2 .. 10]
    cols <- Q.elements [2 .. 10]
    matrixM rows cols (const arbitrary)

instance Eq a => EqProp (Matrix a) where
  (=-=) = eq
