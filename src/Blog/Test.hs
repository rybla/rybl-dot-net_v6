{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Test (main) where

import Blog.Parse (parsePost)
import Blog.Paths
import Blog.Utility (prettyPandoc)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Text.IO (readFile)
import Text.Pandoc (Inline (..), queryWith)
import Text.PrettyPrint.HughesPJClass (hang, nest, render, text, vcat, ($+$), (<+>))
import Prelude hiding (readFile)

main :: IO ()
main = do
  --
  putStrLn "listFilePaths_post ==>"
  listFilePaths_post >>= traverse_ (putStrLn . ("  - " ++))
  --
  listFilePaths_post >>= traverse_ \fp -> do
    txt <- readFile fp
    pandoc <- parsePost txt & runExceptT >>= either (throwError . userError . show) return
    putStrLn . render $ prettyPandoc pandoc
    putStrLn . render $
      ( "links:"
          $+$ ( nest 4 . vcat . fmap (text . show) $
                  pandoc & queryWith \case
                    e@(Link _attr kids (url, _target)) -> [(kids, url)]
                    e@(Image _attr kids (url, _target)) -> [(kids, url)]
                    _ -> []
              )
      )
    return ()
  --
  return ()
