{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module RayTracer.Matrix
  ( Matrix(..)
  , (!)
  , update
  , (//)
  , elemAt
  , matrix
  , fromList
  , fromLists
  , toLists
  , elementwise
  , fromTupleRow
  , fromTupleCol
  , (|*>)
  , (<*|)
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
  )
where

import RayTracer.Tuple

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers (EqProp, eq, (=-=))

import qualified Data.Vector as V
import Data.List (intercalate)
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

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix rows cols f =
  M { rows
    , cols
    , elements = V.fromList [f (r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
    }

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

isInvertable :: (Num a, Eq a) => Matrix a -> Bool
isInvertable m = determinant m /= 0

inverse :: (Fractional a, Eq a) => Matrix a -> Maybe (Matrix a)
inverse m
  | not $ isInvertable m = Nothing
  | otherwise =
    let d = determinant m
    in Just $ matrix (rows m) (cols m) (\(i, j) -> cofactor m j i / d)

translation :: Num a => a -> a -> a -> Matrix a
translation x y z = fromLists [ [1, 0, 0, x]
                              , [0, 1, 0, y]
                              , [0, 0, 1, z]
                              , [0, 0, 0, 1]
                              ]

scaling :: Num a => a -> a -> a -> Matrix a
scaling x y z = fromLists [ [x, 0, 0, 0]
                          , [0, y, 0, 0]
                          , [0, 0, z, 0]
                          , [0, 0, 0, 1]
                          ]

rotationX :: Floating a => a -> Matrix a
rotationX r = fromLists [ [1, 0, 0, 0]
                        , [0, cos r, -sin r, 0]
                        , [0, sin r, cos r, 0]
                        , [0, 0, 0, 1]
                        ]

rotationY :: Floating a => a -> Matrix a
rotationY r = fromLists [ [cos r, 0, sin r, 0]
                        , [0, 1, 0, 0]
                        , [-sin r, 0, cos r, 0]
                        , [0, 0, 0, 1]
                        ]

rotationZ :: Floating a => a -> Matrix a
rotationZ r = fromLists [ [cos r, -sin r, 0, 0]
                        , [sin r, cos r, 0, 0]
                        , [0, 0, 1, 0]
                        , [0, 0, 0, 1]
                        ]

shearing :: Num a => a -> a -> a -> a -> a -> a -> Matrix a
shearing xy xz yx yz zx zy =
  fromLists [ [1, xy, xz, 0]
            , [yx, 1, yz, 0]
            , [zx, zy, 1, 0]
            , [0, 0, 0, 1]
            ]

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    rows <- arbitrary
    cols <- arbitrary
    elements <- V.fromList <$> mapM (const arbitrary) [0..rows * cols]
    return $ M { rows, cols, elements }

instance Eq a => EqProp (Matrix a) where
  (=-=) = eq
