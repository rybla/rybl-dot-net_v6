{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Print.Library (printLibrary) where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Print.Common
import Blog.Process.Common (renderPostHeader)
import Blog.Utility
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Default (def)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import Text.Pandoc (Pandoc)
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printLibrary ::
  (MonadIO m, MonadError Doc m, MonadState env m) =>
  [Post] ->
  m ()
printLibrary posts = evalIsoStateT (pairIso def) do
  -- sort posts by pubDate
  posts <- pure $ List.sortBy (\p1 p2 -> p2._postPubDate `compare` p1._postPubDate) posts

  library <- makeLibrary posts

  templateText <- TextIO.readFile (Paths.offlineSite.template.here </> ("library" & toHtmlFileName)) & liftIO

  libraryTemplate <-
    Pandoc.compileTemplate mempty templateText
      & unBlogTemplateMonad
      >>= fromEither (("compileTemplate:" <+>) . text)

  libraryHtml <- do
    vars <-
      Aeson.parseEither
        Aeson.parseJSON
        ( Aeson.object
            [ ("title", "Library")
            ]
        )
        & fromEither (("Error when parsing template variables JSON:" <+>) . text)
    Pandoc.writeHtml5String
      (commonWriterOptions (Just libraryTemplate) vars)
      library
      & Pandoc.lensPandocM _1

  TextIO.writeFile (Paths.offlineSite.page.here </> ("library" & toHtmlFileName)) libraryHtml & liftIO

makeLibrary ::
  (MonadIO m) =>
  [Post] ->
  m Pandoc
makeLibrary posts = do
  postCards <- posts & traverse makePostCard
  return $
    Pandoc.Pandoc
      mempty
      ( concat
          [postCards]
      )

makePostCard :: (Monad m) => Post -> m Pandoc.Block
makePostCard post = do
  return
    $ Pandoc.Div
      (mempty & Pandoc.attrClasses %~ (["post-card"] ++))
    $ renderPostHeader False post
