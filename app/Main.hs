module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Example.ShadedSphere

main :: IO ()
main = do
  T.writeFile "img/shaded_sphere.ppm" run
