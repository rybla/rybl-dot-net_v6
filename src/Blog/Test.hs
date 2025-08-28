{-# OPTIONS_GHC -Wno-unused-imports #-}

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
  putStrLn "listPostFilePaths ==>"
  listPostFilePaths >>= traverse_ (putStrLn . ("  - " ++))
  --
  listPostFilePaths >>= traverse_ \fp -> do
    txt <- readFile fp
    pandoc <- parsePost txt & runExceptT >>= either (throwError . userError . show) return
    putStrLn . render $ prettyPandoc pandoc
    putStrLn . render $
      ( "links:"
          $+$ ( nest 4 . vcat . fmap (text . show) $
                  pandoc & queryWith \case
                    _e@(Link _attr kids (url, _target)) -> [(kids, url)]
                    _e@(Image _attr kids (url, _target)) -> [(kids, url)]
                    _ -> []
              )
      )
    return ()
  --
  return ()
