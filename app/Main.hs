module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Example.Plane

main :: IO ()
main = do
  T.writeFile "img/plane.ppm" run
