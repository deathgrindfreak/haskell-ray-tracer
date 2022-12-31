module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Example.SimpleSphere

main :: IO ()
main = do
  T.writeFile "img/simple_sphere.ppm" run
