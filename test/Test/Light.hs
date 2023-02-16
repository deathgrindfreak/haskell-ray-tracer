module Test.Light (spec_Light) where

import           Test.Helper.Approximate
import           RayTracer.Color
import           RayTracer.Light
import           RayTracer.Tuple
import           Test.Hspec

spec_Light :: Spec
spec_Light = describe "Light" $ do
  let m = defaultMaterial
      position = Point 0 0 0

  it "Lighting with the eye between the light and the surface" $ do
    let eyev = Vec 0 0 (-1)
        normalv = Vec 0 0 (-1)
        light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
        result = lighting m light position eyev normalv
    result `shouldApproximate` Color 1.9 1.9 1.9

  it "Lighting with the eye between the light and the surface, eye offset 45°" $ do
    let eyev = Vec 0 (sqrt 2 / 2) (-sqrt 2 / 2)
        normalv = Vec 0 0 (-1)
        light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
        result = lighting m light position eyev normalv
    result `shouldApproximate` Color 1.0 1.0 1.0

  it "Lighting with the opposite the surface, light offset 45°" $ do
    let eyev = Vec 0 0 (-1)
        normalv = Vec 0 0 (-1)
        light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
        result = lighting m light position eyev normalv
    result `shouldApproximate` Color 0.7364 0.7364 0.7364

  it "Lighting with the eye in the path of the reflection vector" $ do
    let eyev = Vec 0 (-sqrt 2 / 2) (-sqrt 2 / 2)
        normalv = Vec 0 0 (-1)
        light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
        result = lighting m light position eyev normalv
    result `shouldApproximate` Color 1.6364 1.6364 1.6364

  it "Lighting with the light behind the surface" $ do
    let eyev = Vec 0 0 (-1)
        normalv = Vec 0 0 (-1)
        light = PointLight (Point 0 0 10) (Color 1 1 1)
        result = lighting m light position eyev normalv
    result `shouldApproximate` Color 0.1 0.1 0.1
