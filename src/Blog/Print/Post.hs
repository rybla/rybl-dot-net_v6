{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Print.Post where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Print.Common
import Blog.Utility
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Default (def)
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Highlighting as Highlighting
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printPost :: (MonadIO m, MonadError Doc m, MonadState env m) => Post -> m ()
printPost post = evalIsoStateT (pairIso def) do
  templateText <- TextIO.readFile (Paths.offlineSite.template.here </> ("post" & toHtmlFileName)) & liftIO

  contentHtml <-
    Pandoc.writeHtml5String
      Pandoc.def
        { Pandoc.writerHTMLMathMethod = Pandoc.MathJax "",
          Pandoc.writerHighlightStyle = Just Highlighting.espresso
        }
      post._postDoc
      & Pandoc.lensPandocM _1

  postTemplate <-
    Pandoc.compileTemplate mempty templateText
      & unBlogTemplateMonad
      >>= fromEither (("compileTemplate:" <+>) . text)

  postHtml <- do
    vars <-
      Aeson.parseEither
        Aeson.parseJSON
        ( Aeson.object
            [ ("title", post._postTitle & Aeson.toJSON),
              ("tags", post._postTags & Aeson.toJSON),
              ("content", contentHtml & Aeson.toJSON)
            ]
        )
        & fromEither (("Error when parsing template variables JSON:" <+>) . text)
    Pandoc.writeHtml5String
      Pandoc.def
        { Pandoc.writerTemplate = Just postTemplate,
          Pandoc.writerVariables = vars,
          Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
        }
      (post ^. postDoc)
      & Pandoc.lensPandocM _1

  TextIO.writeFile (post._postId & toPostFilePath) postHtml & liftIO
