module Test.World (spec_World) where

import           RayTracer.World
import           Test.Hspec

spec_World :: Spec
spec_World = describe "World" $ do
  it "" $ do
    let _world = defaultWorld
    True `shouldBe` True
