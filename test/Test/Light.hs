module Test.Light (test_Light) where

import RayTracer.Color
import RayTracer.Light hiding (position)
import RayTracer.Tuple
import Test.Helper.Approximate

import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

test_Light :: Tasty.TestTree
test_Light =
  let m = defaultMaterial
      position = Point 0 0 0
   in Tasty.testGroup
        "Light"
        [ THH.testProperty "Lighting with the eye between the light and the surface" $
            HH.property $ do
              let eyev = Vec 0 0 (-1)
                  normalv = Vec 0 0 (-1)
                  light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                  result = lighting m light position eyev normalv False
              result ~== Color 1.9 1.9 1.9
        , THH.testProperty "Lighting with the eye between the light and the surface, eye offset 45°" $
            HH.property $ do
              let eyev = Vec 0 (sqrt 2 / 2) (-sqrt 2 / 2)
                  normalv = Vec 0 0 (-1)
                  light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                  result = lighting m light position eyev normalv False
              result ~== Color 1.0 1.0 1.0
        , THH.testProperty "Lighting with the opposite the surface, light offset 45°" $
            HH.property $ do
              let eyev = Vec 0 0 (-1)
                  normalv = Vec 0 0 (-1)
                  light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
                  result = lighting m light position eyev normalv False
              result ~== Color 0.7364 0.7364 0.7364
        , THH.testProperty "Lighting with the eye in the path of the reflection vector" $
            HH.property $ do
              let eyev = Vec 0 (-sqrt 2 / 2) (-sqrt 2 / 2)
                  normalv = Vec 0 0 (-1)
                  light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
                  result = lighting m light position eyev normalv False
              result ~== Color 1.6364 1.6364 1.6364
        , THH.testProperty "Lighting with the light behind the surface" $
            HH.property $ do
              let eyev = Vec 0 0 (-1)
                  normalv = Vec 0 0 (-1)
                  light = PointLight (Point 0 0 10) (Color 1 1 1)
                  result = lighting m light position eyev normalv False
              result ~== Color 0.1 0.1 0.1
        , THH.testProperty "Lighting with the surface in shadow" $
            HH.property $ do
              let eyev = Vec 0 0 (-1)
                  normalv = Vec 0 0 (-1)
                  light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                  inShadow = True
                  result = lighting m light position eyev normalv inShadow
              result ~== Color 0.1 0.1 0.1
        ]
