{-# OPTIONS_GHC -Wno-unused-imports #-}

module Blog.Test (main) where

import Blog.Pandoc (runPandocM)
import Blog.Parse (parsePost)
import Blog.Paths
import Blog.Utility (fromEither, prettyPandoc)
import Control.Monad ((>=>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans (lift)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Text.IO (readFile)
import Service.Favicon.Favicone (FaviconeService)
import Text.Pandoc (Inline (..), queryWith)
import Text.PrettyPrint.HughesPJClass
import Prelude hiding (readFile)

main :: IO ()
main = (runExceptT >=> either (throwError . userError . render . text . show) return) do
  --
  lift $ putStrLn "listPostFilePaths ==>"
  listPostFilePaths >>= traverse_ (lift . putStrLn . ("  - " ++))
  --
  listPostFilePaths >>= traverse_ \fp -> do
    txt <- lift $ readFile fp
    pandoc <- parsePost @FaviconeService txt & runPandocM
    lift . putStrLn . render $ prettyPandoc pandoc
    lift . putStrLn . render $
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
