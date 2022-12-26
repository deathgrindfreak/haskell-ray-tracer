module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Example.Projectile

main :: IO ()
main = do
  T.writeFile "projectile.ppm" run
