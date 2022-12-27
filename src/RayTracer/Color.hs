module RayTracer.Color
  ( Color(..)
  ) where

data Color a = Color
  { red :: a
  , green :: a
  , blue :: a
  }
  deriving (Show, Eq)

instance Functor Color where
  fmap f (Color a b c) = Color (f a) (f b) (f c)

instance Applicative Color where
  pure a = Color a a a
  (Color f g h) <*> (Color a b c) = Color (f a) (g b) (h c)

instance (Num a) =>  Num (Color a) where
  negate = fmap negate
  a + b = (+) <$> a <*> b
  a * b = (*) <$> a <*> b
  fromInteger i = Color i' i' i' where i' = fromInteger i
  abs = fmap abs
  signum = fmap signum
