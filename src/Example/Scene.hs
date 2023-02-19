module Example.Scene (run) where

import Data.Text.Lazy (Text)
import qualified Data.Vector as V
import RayTracer.Camera hiding (transform)
import RayTracer.Canvas
import RayTracer.Color
import RayTracer.Light
import RayTracer.Ray
import RayTracer.Transform
import RayTracer.Tuple
import RayTracer.World

run :: Text
run =
  let view =
        viewTransform
          (Point 0 1.5 (-5))
          (Point 0 1 0)
          (Vec 0 1 0)
      camera = mkCamera 640 360 (pi / 3) view

      wallMaterial =
        defaultMaterial
          { materialColor = Color 1 0.9 0.9
          , specular = 0
          }

      sceneFloor =
        (makeSphere 0)
          { transform = scaling 10 0.01 10
          , material = wallMaterial
          }

      leftWall =
        (makeSphere 1)
          { transform =
              scaling 10 0.01 10
                |> rotationX (pi / 2)
                |> rotationY (-pi / 4)
                |> translation 0 0 5
          , material = wallMaterial
          }

      rightWall =
        (makeSphere 2)
          { transform =
              scaling 10 0.01 10
                |> rotationX (pi / 2)
                |> rotationY (pi / 4)
                |> translation 0 0 5
          , material = wallMaterial
          }

      middle =
        (makeSphere 3)
          { transform = translation (-0.5) 1 0.5
          , material =
              defaultMaterial
                { materialColor = Color 0.1 1 0.5
                , diffuse = 0.7
                , specular = 0.3
                }
          }

      right =
        (makeSphere 4)
          { transform =
              scaling 0.5 0.5 0.5
                |> translation (1.5) 0.5 (-0.5)
          , material =
              defaultMaterial
                { materialColor = Color 0.5 1 0.1
                , diffuse = 0.7
                , specular = 0.3
                }
          }

      left =
        (makeSphere 5)
          { transform =
              scaling 0.33 0.33 0.33
                |> translation (-1.5) 0.33 (-0.75)
          , material =
              defaultMaterial
                { materialColor = Color 1 0.8 0.1
                , diffuse = 0.7
                , specular = 0.3
                }
          }

      world =
        World
          { light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)
          , objects =
              V.fromList
                [ sceneFloor
                , leftWall
                , rightWall
                , middle
                , right
                , left
                ]
          }
   in canvasToPPM $ render camera world
