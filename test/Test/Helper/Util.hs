module Test.Helper.Util (makeSphere) where

import Control.Lens
import RayTracer.Ray

makeSphere :: Int -> Object HasId
makeSphere oId = defaultShape Sphere & objectId .~ oId
