module Test.Planes (test_Planes) where

import Control.Lens
import RayTracer.Ray
import RayTracer.Tuple

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

test_Planes :: Tasty.TestTree
test_Planes =
  Tasty.testGroup
    "Planes"
    [ THH.testProperty "Intersect with a ray parallel to the plane" $
        HH.property $ do
          let p = defaultShape Plane & objectId .~ 100
              r = Ray (Point 0 10 0) (Vec 0 0 1)
              xs = p `intersect` r
          xs === []
    , THH.testProperty "A ray intersecting a plane from above" $
        HH.property $ do
          let p = defaultShape Plane & objectId .~ 11
              r = Ray (Point 0 1 0) (Vec 0 (-1) 0)
              xs = p `intersect` r
          xs === [Intersection p 1]
    , THH.testProperty "A ray intersecting a plane from below" $
        HH.property $ do
          let p = defaultShape Plane & objectId .~ 11
              r = Ray (Point 0 (-1) 0) (Vec 0 1 0)
              xs = p `intersect` r
          xs === [Intersection p 1]
    ]
