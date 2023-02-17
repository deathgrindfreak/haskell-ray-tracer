{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RayTracer.Canvas
  ( Canvas (..),
    Pixel,
    makeCanvas,
    pixelAt,
    writePixel,
    writePixels,
    mapCanvas,
    canvasToPPM,
    toIndex,
    fromIndex,
  ) where

import Data.Bifunctor (first)
import qualified Data.Vector as V
import RayTracer.Color

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B

data Canvas = Canvas
  { width :: !Int
  , height :: !Int
  , pixels :: V.Vector (Color Double)
  }
  deriving (Show)

type Pixel = ((Int, Int), Color Double)

makeCanvas :: (Int, Int) -> Canvas
makeCanvas (w, h) = Canvas w h (V.replicate (w * h) 0)

toIndex :: Canvas -> (Int, Int) -> Int
toIndex Canvas {width} (x, y) = x + width * y

fromIndex :: Canvas -> Int -> (Int, Int)
fromIndex Canvas {width} i = let (y, x) = i `divMod` width in (x, y)

pixelAt :: (Int, Int) -> Canvas -> Color Double
pixelAt (x, y) c@Canvas {pixels} = pixels V.! toIndex c (x, y)

writePixel :: Pixel -> Canvas -> Canvas
writePixel (coord, color) c =
  c {pixels = pixels c V.// [(toIndex c coord, color)]}

writePixels :: [Pixel] -> Canvas -> Canvas
writePixels coords c =
  c {pixels = pixels c V.// map (first (toIndex c)) (filter (inBounds c . fst) coords)}

mapCanvas :: (Pixel -> Pixel) -> Canvas -> Canvas
mapCanvas f c@Canvas {pixels} = c {pixels = V.imap updatePixel pixels}
  where
    updatePixel i color = snd $ f (fromIndex c i, color)

inBounds :: Canvas -> (Int, Int) -> Bool
inBounds c@Canvas {pixels} coord = toIndex c coord < length pixels

canvasToPPM :: Canvas -> T.Text
canvasToPPM Canvas {width, height, pixels} =
  B.toLazyText $ header <> body pixels <> B.singleton '\n'
  where
    header =
      mconcat
        [ B.fromString "P3\n"
        , B.decimal width <> B.singleton ' ' <> B.decimal height <> B.singleton '\n'
        , B.fromString "255\n"
        ]

    body :: V.Vector (Color Double) -> B.Builder
    body =
      (\(f, _, _) -> f)
        . V.ifoldl' breakLines (B.fromString "", 0, True)
        . V.concatMap (\(Color r g b) -> V.fromList $ map (show . toPixel) [r, g, b])

    breakLines :: (B.Builder, Int, Bool) -> Int -> String -> (B.Builder, Int, Bool)
    breakLines (bd, lc, atStart) i color =
      let newLC = lc + length color + if atStart then 0 else 1
          needsNL = i `mod` (width * 3) == 0 || newLC >= 70
          sep =
            if
                | atStart -> B.fromString ""
                | needsNL -> B.singleton '\n'
                | otherwise -> B.singleton ' '
       in (bd <> sep <> B.fromString color, if needsNL then length color else newLC, False)

    toPixel :: Double -> Int
    toPixel p = max 0 . min 255 . round $ 255 * p
