module Test.Transform (test_ViewTransform) where

import qualified RayTracer.Matrix as Matrix
import qualified RayTracer.Transform as Transform
import qualified RayTracer.Tuple as T
import Test.Helper.Approximate ((~==))

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

test_ViewTransform :: Tasty.TestTree
test_ViewTransform =
  Tasty.testGroup
    "View Transform"
    [ THH.testProperty "The transformation matrix for the default orientation" $
        HH.property $ do
          let from = T.Point 0 0 0
              to = T.Point 0 0 (-1)
              up = T.Vec 0 1 0
          Transform.viewTransform from to up === Transform.identityTransform
    , THH.testProperty "The transformation matrix looking in the positive z direction" $
        HH.property $ do
          let from = T.Point 0 0 0
              to = T.Point 0 0 1
              up = T.Vec 0 1 0
          Transform.viewTransform from to up === Transform.scaling (-1) 1 (-1)
    , THH.testProperty "The view transformation moves the world" $
        HH.property $ do
          let from = T.Point 0 0 8
              to = T.Point 0 0 0
              up = T.Vec 0 1 0
          Transform.viewTransform from to up === Transform.translation 0 0 (-8)
    , THH.testProperty "An arbitrary view transformation" $
        HH.property $ do
          let from = T.Point 1 3 2
              to = T.Point 4 (-2) 8
              up = T.Vec 1 1 0
              m =
                Matrix.fromLists
                  [ [-0.50709, 0.50709, 0.67612, -2.36643]
                  , [0.76772, 0.60609, 0.12122, -2.82843]
                  , [-0.35857, 0.59761, -0.71714, 0.00000]
                  , [0.00000, 0.00000, 0.00000, 1.00000]
                  ]
          Transform.toMatrix (Transform.viewTransform from to up) ~== m
    ]
