module Main (main) where

import qualified Blog.Build
import Blog.Utility

main :: IO ()
main = do
  logM "main" "begin"
  Blog.Build.main
  logM "main" "end"
