{-# LANGUAGE NamedFieldPuns #-}

module RayTracer.Matrix
  ( Matrix(..)
  , (!)
  , elemAt
  , toLists
  , fromList
  , fromLists
  , elementwise
  , fromTupleRow
  , fromTupleCol
  , (|*>)
  , (<*|)
  , transpose
  , identity
  )
where

import RayTracer.Tuple

import qualified Data.Vector as V
import Data.List (intercalate, foldl')
import Data.Bifunctor (Bifunctor(first))

data Matrix a = M
  { rows :: Int
  , cols :: Int
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

fromTupleRow :: Num a => Tuple a -> Matrix a
fromTupleRow (Vec a b c) = fromList 1 4 [a, b, c, 0]
fromTupleRow (Point a b c) = fromList 1 4 [a, b, c, 1]
fromTupleRow (Scalar _) = error "Cannot convert Scalar to Matrix"

fromTupleCol :: Num a => Tuple a -> Matrix a
fromTupleCol (Vec a b c) = fromList 4 1 [a, b, c, 0]
fromTupleCol (Point a b c) = fromList 4 1 [a, b, c, 1]
fromTupleCol (Scalar _) = error "Cannot convert Scalar to Matrix"

(|*>) :: (Num a, Eq a) => Matrix a -> Tuple a -> Tuple a
m |*> t =
  if dims m /= (4, 4)
  then error "Dimensions of the matrix must be 4x4"
  else
    let m' = m * fromTupleCol t
     in case m' ! (3, 0) of
      0 -> Vec (m' ! (0, 0)) (m' ! (1, 0)) (m' ! (2, 0))
      1 -> Point (m' ! (0, 0)) (m' ! (1, 0)) (m' ! (2, 0))
      _ -> error "Improper multiplication result"

(<*|) :: (Num a, Eq a) => Tuple a -> Matrix a -> Tuple a
t <*| m =
  if dims m /= (4, 4)
  then error "Dimensions of the matrix must be 4x4"
  else
    let m' = fromTupleRow t * m
     in case m' ! (0, 3) of
      0 -> Vec (m' ! (0, 0)) (m' ! (0, 1)) (m' ! (0, 2))
      1 -> Point (m' ! (0, 0)) (m' ! (0, 1)) (m' ! (0, 2))
      _ -> error "Improper multiplication result"

identity :: Num a => Int -> Matrix a
identity n = fromList n n (repeat 0) // [((i, i), 1) | i <- [0..n-1]]

transpose :: Matrix a -> Matrix a
transpose matrix@M { rows, cols } =
  foldl' swap matrix [(i, j) | i <- [0..rows - 1], j <- [0..cols - 1], i <= j]
  where
    swap m@M{ elements = e } (i, j) =
      let (a, b) = (idx m i j, idx m j i)
       in m { elements = e V.// [(b, e V.! a), (a, e V.! b)] }

idx :: Matrix a -> Int -> Int -> Int
idx M { cols } r c = c + r * cols
