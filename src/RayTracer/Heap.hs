module RayTracer.Heap
  ( LeftistHeap (..)
  , Heap (..)
  , singleton
  , popMin
  )
where

import Control.Applicative (Applicative (liftA2))

data LeftistHeap a
  = Empty
  | Tree Int a (LeftistHeap a) (LeftistHeap a)
  deriving (Show)

class Heap h where
  empty :: Ord a => h a
  isEmpty :: Ord a => h a -> Bool
  insert :: Ord a => a -> h a -> h a
  merge :: Ord a => h a -> h a -> h a
  findMin :: Ord a => h a -> Maybe a
  deleteMin :: Ord a => h a -> Maybe (h a)

instance Heap LeftistHeap where
  empty = Empty

  isEmpty Empty = True
  isEmpty _ = False

  merge Empty h = h
  merge h Empty = h
  merge t1@(Tree _ x l1 r1) t2@(Tree _ y l2 r2) =
    if x <= y
      then makeTree x l1 (merge r1 t2)
      else makeTree y l2 (merge t1 r2)
    where
      rank Empty = 0
      rank (Tree r _ _ _) = r

      makeTree a l r =
        if rank l >= rank r
          then Tree (rank r + 1) a l r
          else Tree (rank l + 1) a r l

  insert x = merge (Tree 1 x Empty Empty)

  findMin Empty = Nothing
  findMin (Tree _ x _ _) = Just x

  deleteMin Empty = Nothing
  deleteMin (Tree _ _ l r) = Just (merge l r)

singleton :: (Heap h, Ord a) => a -> h a
singleton a = insert a empty

popMin :: (Heap h, Ord a) => h a -> Maybe (a, h a)
popMin h = liftA2 (,) (findMin h) (deleteMin h)
