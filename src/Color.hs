module Color
  ( Color(..)
  ) where

data Color a = Color
  { red :: a
  , green :: a
  , blue :: a
  }
  deriving (Show, Eq)

instance (Num a) =>  Num (Color a) where
  negate (Color r g b) = Color (-r) (-g) (-b)
  Color r1 g1 b1 + Color r2 g2 b2 = Color (r1 + r2) (g1 + g2) (b1 + b2)
  Color r1 g1 b1 * Color r2 g2 b2 = Color (r1 * r2) (g1 * g2) (b1 * b2)
  fromInteger i = Color i' i' i' where i' = fromInteger i
  abs _ = error "Unsupported operation"
  signum _ = error "Unsupported operation"
