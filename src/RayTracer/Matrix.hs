{-# LANGUAGE NamedFieldPuns #-}

module RayTracer.Matrix
  ( Matrix(..)
  , (!)
  , toLists
  , fromList
  , fromLists
  , elementwise
  )
where

import qualified Data.Vector as V
import Data.List (intercalate)

data Matrix a = M
  { rows :: Int
  , cols :: Int
  , elements :: V.Vector a
  }

instance (Num a, Ord a, Show a) => Show (Matrix a) where
  show = prettyPrintMatrix

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

elementwise :: (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
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
  (*) = undefined
  fromInteger = M 1 1 . V.singleton . fromInteger

(!) :: Matrix a -> (Int, Int) -> a
(!) m@M { elements } (i, j) = elements V.! idx m i j

toLists :: Matrix a -> [[a]]
toLists m@M { rows, cols } = [[m ! (i, j) | j <- [0..cols-1]] | i <- [0..rows-1]]

fromList :: (Int, Int) -> [a] -> Matrix a
fromList (rows, cols) lst =
  let elements = take (cols * rows) lst
  in if cols * rows > length elements
       then error "List does not correspond to dimensions"
       else M { rows, cols, elements = V.fromList elements }

fromLists :: [[a]] -> Matrix a
fromLists [] = error "Empty list"
fromLists l@(xs:_) = fromList (length l, length xs) (concat l)

idx :: Matrix a -> Int -> Int -> Int
idx M { cols } r c = c + r * cols
