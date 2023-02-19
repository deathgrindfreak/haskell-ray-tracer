module Test.World (test_World) where

import qualified RayTracer.Camera as Camera
import qualified RayTracer.Canvas as Canvas
import RayTracer.Color
import qualified RayTracer.Light as L
import qualified RayTracer.Ray as R
import qualified RayTracer.Transform as M
import qualified RayTracer.Tuple as T
import qualified RayTracer.World as W
import Test.Helper.Approximate ((~==))
import Test.Helper.Util

import qualified Data.Vector as V

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

test_World :: Tasty.TestTree
test_World =
  Tasty.testGroup
    "World"
    [ THH.testProperty "Intersect a world with a ray" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)

          map R.t (W.intersectWorld r defaultWorld) === [4, 4.5, 5.5, 6]
    , THH.testProperty "Precomputing the state of an intersection" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)
              shape = makeSphere 0
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
              shape = makeSphere 0
              i = R.Intersection shape 4
              comps = W.prepareComputations i r

          W.inside comps === False
    , THH.testProperty "The hit, when an intersection occurs on the inside" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 0) (T.Vec 0 0 1)
              shape = makeSphere 0
              i = R.Intersection shape 1
              comps = W.prepareComputations i r

          W.point comps === T.Point 0 0 1
          W.eyev comps === T.Vec 0 0 (-1)
          W.normalv comps === T.Vec 0 0 (-1)
          W.inside comps === True
    , THH.testProperty "Shading an intersection" $
        HH.property $ do
          let w = defaultWorld
              r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)
              shape = W.objects w V.! 0
              i = R.Intersection shape 4
              comps = W.prepareComputations i r
          W.shadeHit w comps ~== Color 0.38066 0.47583 0.2855
    , THH.testProperty "Shading an intersection from the inside" $
        HH.property $ do
          let w = defaultWorld {W.light = L.PointLight (T.Point 0 0.25 0) (Color 1 1 1)}
              r = R.Ray (T.Point 0 0 0) (T.Vec 0 0 1)
              shape = W.objects w V.! 1
              i = R.Intersection shape 0.5
              comps = W.prepareComputations i r
          W.shadeHit w comps ~== Color 0.90498 0.90498 0.90498
    , THH.testProperty "The color when a ray misses" $
        HH.property $ do
          let w = defaultWorld
              r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 1 0)
          W.colorAt w r === Color 0 0 0
    , THH.testProperty "The color when a ray hits" $
        HH.property $ do
          let w = defaultWorld
              r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)
          W.colorAt w r ~== Color 0.38066 0.47583 0.2855
    , THH.testProperty "The color with an intersection behind the ray" $
        HH.property $ do
          let w = lightWorld
              inner = W.objects w V.! 1
              r = R.Ray (T.Point 0 0 0.75) (T.Vec 0 0 (-1))
          W.colorAt w r === L.materialColor (R.material inner)
    , THH.testProperty "" $
        HH.property $ do
          let w = defaultWorld
              from = T.Point 0 0 (-5)
              to = T.Point 0 0 0
              up = T.Vec 0 1 0
              t = M.viewTransform from to up
              c = Camera.mkCamera 11 11 (pi / 2) t
              image = Camera.render c w
          Canvas.pixelAt (5, 5) image ~== Color 0.38066 0.47583 0.2855
    ]

defaultWorld :: W.World
defaultWorld =
  let materialLarger =
        L.defaultMaterial
          { L.materialColor = Color 0.8 1.0 0.6
          , L.diffuse = 0.7
          , L.specular = 0.2
          }
   in W.World
        { W.light = L.PointLight (T.Point (-10) 10 (-10)) (Color 1 1 1)
        , W.objects =
            V.fromList
              [ (makeSphere 0) {R.material = materialLarger}
              , (makeSphere 1) {R.transform = M.scaling 0.5 0.5 0.5}
              ]
        }

lightWorld :: W.World
lightWorld =
  defaultWorld {W.objects = V.map (setAmbient 1) (W.objects defaultWorld)}
  where
    setAmbient a o =
      let m = R.material o
       in o {R.material = m {L.ambient = a}}
