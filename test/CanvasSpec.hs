{-# LANGUAGE OverloadedStrings #-}

module CanvasSpec (spec) where

import           RayTracer.Canvas
import           RayTracer.Color
import           SpecHelper

import           Control.Monad         (forM_)
import qualified Data.Text.Lazy        as T
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       (Arbitrary (arbitrary), elements)

newtype Dimension = Dimension Int deriving (Show, Eq, Ord)

instance Arbitrary Dimension where
  arbitrary = Dimension <$> elements [1..1000]

spec :: Spec
spec = describe "Canvas" $ do
  prop "toIndex and fromIndex should be inverses" $
    \(Dimension w :: Dimension, Dimension h :: Dimension, i :: Word) -> do
      let c = makeCanvas (w, h)
          i' = fromInteger . toInteger $ i
      toIndex c (fromIndex c i') `shouldBe` i'

  it "Construction" $ do
    let canvas = makeCanvas (10, 20)
    width canvas `shouldBe` 10
    height canvas `shouldBe` 20

    forM_ [0..height canvas - 1] $ \y -> do
      forM_ [0..width canvas - 1] $ \x -> do
        pixelAt (x, y) canvas `shouldBe` 0

  it "Pixel Operations" $ do
    let canvas = makeCanvas (10, 20)
        redColor = Color 1 0 0
        canvas' = writePixel ((2, 3), redColor) canvas
    pixelAt (2, 3) canvas' `shouldBe` redColor

  it "PPM Header" $ do
    let canvas = makeCanvas (5, 3)
        header = T.unlines . take 3 . T.lines $ canvasToPPM canvas
    header `shouldBe` "P3\n5 3\n255\n"

  it "PPM Body" $ do
    let c1 = Color 1.5 0 0
        c2 = Color 0 0.5 0
        c3 = Color (-0.5) 0 1
        canvas = writePixel ((0, 0), c1) . writePixel ((2, 1), c2)
          . writePixel ((4, 2), c3) $ makeCanvas (5, 3)
        body = T.unlines . drop 3 . T.lines $ canvasToPPM canvas
    body `shouldBe` "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n\
                    \0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n\
                    \0 0 0 0 0 0 0 0 0 0 0 0 0 0 255\n"

  it "PPM: Ensure row width is smaller than 70 characters" $ do
    let c = Color 1 0.8 0.6
        canvas = writePixels [((x, y), c) | x <- [0..9], y <- [0..1]] $ makeCanvas (10, 2)
        ppm = canvasToPPM canvas
    ppm `shouldBe` "P3\n\
                   \10 2\n\
                   \255\n\
                   \255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n\
                   \153 255 204 153 255 204 153 255 204 153 255 204 153\n\
                   \255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n\
                   \153 255 204 153 255 204 153 255 204 153 255 204 153\n"

  it "PPM: ends with newline character" $ do
    T.last (canvasToPPM (makeCanvas (5, 3))) `shouldBe` '\n'
