{-# LANGUAGE TemplateHaskell #-}

module RayTracer.Light
  ( PointLight (..)
  , position
  , intensity
  , Material (..)
  , color
  , ambient
  , diffuse
  , specular
  , shininess
  , defaultMaterial
  , lighting
  )
where

import Control.Lens
import RayTracer.Color
import RayTracer.Tuple

data PointLight = PointLight
  { _position :: Point Double
  , _intensity :: Color Double
  }
  deriving (Show)

data Material = Material
  { _color :: Color Double
  , _ambient :: Double
  , _diffuse :: Double
  , _specular :: Double
  , _shininess :: Double
  }
  deriving (Eq, Show)

makeLenses ''PointLight
makeLenses ''Material

defaultMaterial :: Material
defaultMaterial =
  Material
    { _color = Color 1 1 1
    , _ambient = 0.1
    , _diffuse = 0.9
    , _specular = 0.9
    , _shininess = 200.0
    }

lighting ::
  Material ->
  PointLight ->
  Point Double ->
  Vec Double ->
  Vec Double ->
  Color Double
lighting m light point eyev normalv =
  let effectiveColor = m ^. color * light ^. intensity

      -- Find the direction to the light source
      lightv = norm (light ^. position |-| point)

      -- Compute the ambient contribution
      ambientColor = effectiveColor * toColor (m ^. ambient)

      -- If lightDotNormal is negative, then the light source is on the other side of the surface
      lightDotNormal = fromScalar $ lightv |*| normalv

      black = Color 0 0 0

      diffuseColor =
        if lightDotNormal < 0
          then black
          else effectiveColor * toColor (m ^. diffuse * lightDotNormal)

      specularColor =
        if lightDotNormal < 0
          then black
          else
            let reflectv = reflect (neg lightv) normalv
                reflectDotEye = fromScalar $ reflectv |*| eyev
             in if reflectDotEye <= 0
                  then black
                  else
                    let factor = reflectDotEye ** (m ^. shininess)
                     in (light ^. intensity) * toColor (m ^. specular * factor)
   in ambientColor + diffuseColor + specularColor
