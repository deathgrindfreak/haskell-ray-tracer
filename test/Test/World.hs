module Test.World (test_World) where

import           qualified RayTracer.World as W
import           qualified RayTracer.Ray as R
import           qualified RayTracer.Tuple as T

import Hedgehog ((===))
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH
import qualified Hedgehog as HH

test_World :: Tasty.TestTree
test_World =
  Tasty.testGroup "World"
    [ THH.testProperty "Intersect a world with a ray" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)

          map R.t (W.intersectWorld r W.defaultWorld) === [4, 4.5, 5.5, 6]

    , THH.testProperty "Precomputing the state of an intersection" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)
              shape = R.makeSphere 0
              i = R.Intersection shape 4
              comps = W.prepareComputations i r

          W.t comps === R.t i
          W.object comps === R.object i
          W.point comps === T.Point 0 0 (-1)
          W.eyev comps === T.Vec 0 0 (-1)
          W.normalv comps === T.Vec 0 0 (-1)

    , THH.testProperty "The hit, when an intersection occurs on the outside" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)
              shape = R.makeSphere 0
              i = R.Intersection shape 4
              comps = W.prepareComputations i r

          W.inside comps === False

    , THH.testProperty "The hit, when an intersection occurs on the inside" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 0) (T.Vec 0 0 1)
              shape = R.makeSphere 0
              i = R.Intersection shape 1
              comps = W.prepareComputations i r

          W.point comps === T.Point 0 0 1
          W.eyev comps === T.Vec 0 0 (-1)
          W.normalv comps === T.Vec 0 0 (-1)
          W.inside comps === True
    ]
