module RayTracer.Light
  ( PointLight (..)
  , Material (..)
  , defaultMaterial
  , lighting
  )
where

import RayTracer.Color
import RayTracer.Tuple

data PointLight = PointLight
  { lightPosition :: Point Double
  , intensity :: Color Double
  }
  deriving (Show)

data Material = Material
  { materialColor :: Color Double
  , ambient :: Double
  , diffuse :: Double
  , specular :: Double
  , shininess :: Double
  }
  deriving (Eq, Show)

defaultMaterial :: Material
defaultMaterial =
  Material
    { materialColor = Color 1 1 1
    , ambient = 0.1
    , diffuse = 0.9
    , specular = 0.9
    , shininess = 200.0
    }

lighting ::
  Material ->
  PointLight ->
  Point Double ->
  Vec Double ->
  Vec Double ->
  Color Double
lighting m light point eyev normalv =
  let effectiveColor = materialColor m * intensity light

      -- Find the direction to the light source
      lightv = norm (lightPosition light |-| point)

      -- Compute the ambient contribution
      ambientColor = effectiveColor * toColor (ambient m)

      -- If lightDotNormal is negative, then the light source is on the other side of the surface
      lightDotNormal = fromScalar $ lightv |*| normalv

      black = Color 0 0 0

      diffuseColor =
        if lightDotNormal < 0
          then black
          else effectiveColor * toColor (diffuse m * lightDotNormal)

      specularColor =
        if lightDotNormal < 0
          then black
          else
            let reflectv = reflect (neg lightv) normalv
                reflectDotEye = fromScalar $ reflectv |*| eyev
             in if reflectDotEye <= 0
                  then black
                  else
                    let factor = reflectDotEye ** shininess m
                     in intensity light * toColor (specular m * factor)
   in ambientColor + diffuseColor + specularColor
