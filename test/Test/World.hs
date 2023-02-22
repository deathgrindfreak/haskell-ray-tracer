module Test.World (test_World) where

import Control.Lens hiding (from, to)
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

          map (^. R.t) (W.intersectWorld r defaultWorld) === [4, 4.5, 5.5, 6]
    , THH.testProperty "Precomputing the state of an intersection" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)
              shape = makeSphere 0
              i = R.Intersection shape 4
              comps = W.prepareComputations i r

          comps ^. W.intersection . R.t === i ^. R.t
          comps ^. W.intersection . R.object === i ^. R.object
          comps ^. W.point === T.Point 0 0 (-1)
          comps ^. W.eyev === T.Vec 0 0 (-1)
          comps ^. W.normalv === T.Vec 0 0 (-1)
    , THH.testProperty "The hit, when an intersection occurs on the outside" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)
              shape = makeSphere 0
              i = R.Intersection shape 4
              comps = W.prepareComputations i r

          comps ^. W.inside === False
    , THH.testProperty "The hit, when an intersection occurs on the inside" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 0) (T.Vec 0 0 1)
              shape = makeSphere 0
              i = R.Intersection shape 1
              comps = W.prepareComputations i r

          comps ^. W.point === T.Point 0 0 1
          comps ^. W.eyev === T.Vec 0 0 (-1)
          comps ^. W.normalv === T.Vec 0 0 (-1)
          comps ^. W.inside === True
    , THH.testProperty "Shading an intersection" $
        HH.property $ do
          let w = defaultWorld
              r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)
              shape = (w ^. W.objects) V.! 0
              i = R.Intersection shape 4
              comps = W.prepareComputations i r
          W.shadeHit w comps ~== Color 0.38066 0.47583 0.2855
    , THH.testProperty "Shading an intersection from the inside" $
        HH.property $ do
          let w = defaultWorld & W.light .~ L.PointLight (T.Point 0 0.25 0) (Color 1 1 1)
              r = R.Ray (T.Point 0 0 0) (T.Vec 0 0 1)
              shape = (w ^. W.objects) V.! 1
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
              inner = (w ^. W.objects) V.! 1
              r = R.Ray (T.Point 0 0 0.75) (T.Vec 0 0 (-1))
          W.colorAt w r === inner ^. R.material . L.color
    , THH.testProperty "Rendering a world with a camera" $
        HH.property $ do
          let w = defaultWorld
              from = T.Point 0 0 (-5)
              to = T.Point 0 0 0
              up = T.Vec 0 1 0
              t = M.viewTransform from to up
              c = Camera.mkCamera 11 11 (pi / 2) t
              image = Camera.render c w
          Canvas.pixelAt (5, 5) image ~== Color 0.38066 0.47583 0.2855
    , THH.testProperty "There is no shadow when nothing is collinear with point and light" $
        HH.property $ do
          let w = defaultWorld
              p = T.Point 0 10 0
          W.isShadowed w p === False
    , THH.testProperty "The shadow when an object is between the point and the light" $
        HH.property $ do
          let w = defaultWorld
              p = T.Point 10 (-10) 10
          W.isShadowed w p === True
    , THH.testProperty "There is no shadow when an object is behind the light" $
        HH.property $ do
          let w = defaultWorld
              p = T.Point (-20) 20 (-20)
          W.isShadowed w p === False
    , THH.testProperty "There is no shadow when an object is behind the point" $
        HH.property $ do
          let w = defaultWorld
              p = T.Point (-2) 2 (-2)
          W.isShadowed w p === False
    , THH.testProperty "shadeHit is given an intersection in shadow" $
        HH.property $ do
          let w =
                W.mkWorld
                  (L.PointLight (T.Point 0 0 (-10)) (Color 1 1 1))
                  [ R.defaultShape R.Sphere
                  , R.defaultShape R.Sphere & R.transform .~ M.translation 0 0 10
                  ]
              r = R.Ray (T.Point 0 0 5) (T.Vec 0 0 1)
              i = R.Intersection ((w ^. W.objects) V.! 1) 4
              comps = W.prepareComputations i r
          W.shadeHit w comps ~== Color 0.1 0.1 0.1
    , THH.testProperty "The hit should offset the point" $
        HH.property $ do
          let r = R.Ray (T.Point 0 0 (-5)) (T.Vec 0 0 1)
              shape =
                R.defaultShape R.Sphere
                  & R.objectId .~ 0
                  & R.transform .~ M.translation 0 0 1
              i = R.Intersection shape 5
              comps = W.prepareComputations i r
          (comps ^. W.overPoint . T.pz < -W.epsilon / 2) === True
          (comps ^. W.point . T.pz > comps ^. W.overPoint . T.pz) === True
    ]

defaultWorld :: W.World
defaultWorld =
  let materialLarger =
        L.defaultMaterial
          & L.color .~ Color 0.8 1.0 0.6
          & L.diffuse .~ 0.7
          & L.specular .~ 0.2
   in W.mkWorld
        (L.PointLight (T.Point (-10) 10 (-10)) (Color 1 1 1))
        [ R.defaultShape R.Sphere & R.material .~ materialLarger
        , R.defaultShape R.Sphere & R.transform .~ M.scaling 0.5 0.5 0.5
        ]

lightWorld :: W.World
lightWorld =
  defaultWorld
    & W.objects
      %~ V.map (\o -> o & R.material . L.ambient .~ 1)
