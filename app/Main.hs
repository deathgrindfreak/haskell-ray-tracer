module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Example.Clock

main :: IO ()
main = do
  T.writeFile "clock.ppm" run
