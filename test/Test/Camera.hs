module Test.Camera (test_Camera) where

import qualified RayTracer.Camera as Camera
import qualified RayTracer.Ray as R
import RayTracer.Transform (identityTransform, rotationY, translation, (|>))
import qualified RayTracer.Tuple as T
import Test.Helper.Approximate (Approx (..), (~==))

import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

test_Camera :: Tasty.TestTree
test_Camera =
  Tasty.testGroup
    "Camera"
    [ THH.testProperty "The pixel size for a horizontal canvas" $
        HH.property $ do
          let camera = Camera.mkCamera 200 125 (pi / 2) identityTransform
          Approx (Camera.pixelSize camera) ~== Approx 0.01
    , THH.testProperty "The pixel size for a vertical canvas" $
        HH.property $ do
          let camera = Camera.mkCamera 125 200 (pi / 2) identityTransform
          Approx (Camera.pixelSize camera) ~== Approx 0.01
    , THH.testProperty "Constructing a ray through the center of the canvas" $
        HH.property $ do
          let camera = Camera.mkCamera 201 101 (pi / 2) identityTransform
              r = Camera.rayForPixel camera 100 50
          r ~== R.Ray (T.Point 0 0 0) (T.Vec 0 0 (-1))
    , THH.testProperty "Constructing a ray through a corner of the canvas" $
        HH.property $ do
          let camera = Camera.mkCamera 201 101 (pi / 2) identityTransform
              r = Camera.rayForPixel camera 0 0
          r ~== R.Ray (T.Point 0 0 0) (T.Vec 0.66519 0.33259 (-0.66851))
    , THH.testProperty "Constructing a ray when the camera is transformed" $
        HH.property $ do
          let t = translation 0 (-2) 5 |> rotationY (pi / 4)
              camera = Camera.mkCamera 201 101 (pi / 2) t
              r = Camera.rayForPixel camera 100 50
          r ~== R.Ray (T.Point 0 2 (-5)) (T.Vec (sqrt 2 / 2) 0 (-sqrt 2 / 2))
    ]
