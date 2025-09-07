module Main (main) where

import qualified Blog.Test as Blog.Test
import qualified Spec.AesonUri

main :: IO ()
main = do
  Blog.Test.main
  Spec.AesonUri.test
