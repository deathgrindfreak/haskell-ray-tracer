{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracer.Camera
  ( Camera (..)
  , mkCamera
  , rayForPixel
  , render
  )
where

import RayTracer.Canvas
import RayTracer.Matrix (inverse)
import RayTracer.Ray (Ray (..))
import RayTracer.Transform (Transform)
import RayTracer.Tuple
import RayTracer.World

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

rayForPixel :: Camera -> Int -> Int -> Ray Double
rayForPixel Camera {..} x y =
  let xoffset = (fromIntegral x + 0.5) * pixelSize
      yoffset = (fromIntegral y + 0.5) * pixelSize
      worldx = halfWidth - xoffset
      worldy = halfHeight - yoffset
      pixel = inverse transform |*| Point worldx worldy (-1)
      origin = inverse transform |*| Point 0 0 0
      direction = norm (pixel |-| origin)
   in Ray origin direction

render :: Camera -> World -> Canvas
render camera@Camera {..} world =
  let image = makeCanvas (hsize, vsize)
   in writePixels
        image
        [ let ray = rayForPixel camera x y
              color = colorAt world ray
           in ((x, y), color)
        | y <- [0 .. vsize - 1]
        , x <- [0 .. hsize - 1]
        ]
