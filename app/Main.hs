module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Example.Scene

main :: IO ()
main = do
  T.writeFile "img/scene.ppm" run
