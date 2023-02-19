{-# LANGUAGE RecordWildCards #-}

module RayTracer.Camera
  ( Camera (..)
  , mkCamera
  , rayForPixel
  )
where

import RayTracer.Matrix (inverse)
import RayTracer.Ray (Ray (..))
import RayTracer.Transform (Transform)
import RayTracer.Tuple

data Camera = Camera
  { hsize :: Int
  , vsize :: Int
  , fieldOfView :: Double
  , pixelSize :: Double
  , halfWidth :: Double
  , halfHeight :: Double
  , transform :: Transform Double
  }
  deriving (Show)

mkCamera :: Int -> Int -> Double -> Transform Double -> Camera
mkCamera hsize vsize fieldOfView transform =
  let halfView = tan (fieldOfView / 2)
      aspect = fromIntegral hsize / fromIntegral vsize
      halfWidth = if aspect >= 1 then halfView else halfView * aspect
      halfHeight = if aspect >= 1 then halfView / aspect else halfView
   in Camera
        { hsize
        , vsize
        , fieldOfView
        , pixelSize = (halfWidth * 2) / fromIntegral hsize
        , halfWidth
        , halfHeight
        , transform = transform
        }

rayForPixel :: Camera -> Double -> Double -> Ray Double
rayForPixel Camera {..} px py =
  let xoffset = (px + 0.5) * pixelSize
      yoffset = (py + 0.5) * pixelSize
      worldx = halfWidth - xoffset
      worldy = halfHeight - yoffset
      pixel = inverse transform |*| Point worldx worldy (-1)
      origin = inverse transform |*| Point 0 0 0
      direction = norm (pixel |-| origin)
   in Ray origin direction
