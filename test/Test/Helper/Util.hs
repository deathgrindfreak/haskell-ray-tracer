module Test.Helper.Util (makeSphere) where

import RayTracer.Ray

makeSphere :: Int -> Object HasId
makeSphere oId = defaultSphere {objectId = oId}
