module RayTracer.World
  ( World(..)
  , defaultWorld
  )
where

import qualified Data.Vector      as V
import           RayTracer.Color
import           RayTracer.Light
import           RayTracer.Matrix
import           RayTracer.Ray
import           RayTracer.Tuple

data World
  = World
      { light   :: PointLight
      , objects :: V.Vector (Object Double)
      }
  deriving (Show)

defaultWorld :: World
defaultWorld =
  let materialLarger =
        defaultMaterial { materialColor = Color 0.8 1.0 0.6
                        , diffuse = 0.7
                        , specular = 0.2
                        }

      mkSphere :: Int -> Object Double
      mkSphere = makeSphere

  in World { light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)
           , objects =
             V.fromList [ (mkSphere 0) { material = materialLarger }
                        , (mkSphere 1) { transform = scaling 0.5 0.5 0.5 :: Transform Double }
                        ]
           }
