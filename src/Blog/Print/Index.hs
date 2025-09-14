{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Print.Index (printIndex) where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Print.Common
import Blog.Process.Common
import Blog.Utility
import Control.Lens hiding (index)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Default (def)
import qualified Data.List as List
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import Text.Pandoc (Pandoc)
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printIndex ::
  (MonadIO m, MonadError Doc m, MonadState env m) =>
  [Post] ->
  m ()
printIndex posts = evalIsoStateT (pairIso def) do
  -- sort posts by pubDate
  posts <- pure $ List.sortBy (\p1 p2 -> p2._postPubDate `compare` p1._postPubDate) posts

  index <- makeIndex posts

  templateText <- TextIO.readFile (Paths.offlineSite.template.here </> ("index" & toHtmlFileName)) & liftIO

  contentHtml <- Pandoc.writeHtml5String Pandoc.def index & Pandoc.lensPandocM _1

  indexTemplate <-
    Pandoc.compileTemplate mempty templateText
      & unBlogTemplateMonad
      >>= fromEither (("compileTemplate:" <+>) . text)

  indexHtml <- do
    vars <-
      Aeson.parseEither
        Aeson.parseJSON
        ( Aeson.object
            [ ("title", "Index"),
              ("content", contentHtml & Aeson.toJSON)
            ]
        )
        & fromEither (("Error when parsing template variables JSON:" <+>) . text)
    Pandoc.writeHtml5String
      ( def
          { Pandoc.writerTemplate = Just indexTemplate,
            Pandoc.writerVariables = vars,
            Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
          }
      )
      index
      & Pandoc.lensPandocM _1

  TextIO.writeFile (Paths.offlineSite.here </> ("index" & toHtmlFileName)) indexHtml & liftIO

makeIndex ::
  (MonadIO m) =>
  [Post] ->
  m Pandoc
makeIndex posts = do
  postCards <- posts & traverse makePostCard
  return $
    Pandoc.Pandoc
      mempty
      ( concat
          [ [ Pandoc.Header 1 mempty [Pandoc.Str "Index"]
            ],
            postCards
          ]
      )

makePostCard :: (Monad m) => Post -> m Pandoc.Block
makePostCard post = do
  return
    $ Pandoc.Div
      (mempty & Pandoc.attrClasses %~ (["post-card"] ++))
    $ renderPostHeader post
