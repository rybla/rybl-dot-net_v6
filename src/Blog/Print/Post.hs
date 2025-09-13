{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Print.Post where

import Blog.Common
import Blog.Pandoc (fromDocError)
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Print.Common
import Blog.Utility (fromEither)
import Control.Lens
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printPost :: (MonadIO m, MonadError Doc m) => Post -> m ()
printPost post = do
  templateText <- TextIO.readFile (Paths.offline.template.here </> ("post" & toHtmlFileName)) & liftIO

  postHtml <- Pandoc.runPandocM do
    contentHtml <-
      Pandoc.writeHtml5String
        Pandoc.def
          { Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
          }
        (post ^. postDoc)

    let varsJson :: Aeson.Value
        varsJson =
          Aeson.object
            [ ("stylesheetHref", "post.css"),
              ("title", post & postTitle & Aeson.toJSON),
              ("tags", post & postTags & Aeson.toJSON),
              ("content", contentHtml & Aeson.toJSON)
            ]

    postTemplate <-
      Pandoc.compileTemplate mempty templateText
        -- & Pandoc.runWithPartials
        & unBlogTemplateMonad
        >>= \case
          Left err -> throwError $ Pandoc.PandocAppError $ Text.pack err
          Right t -> return t

    postHtml <- do
      vars <- Aeson.parseEither Aeson.parseJSON varsJson & fromEither (("Error when parsing template variables JSON:" <+>) . text) & fromDocError
      Pandoc.writeHtml5String
        Pandoc.def
          { Pandoc.writerTemplate = Just postTemplate,
            Pandoc.writerVariables = vars,
            Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
          }
        (post ^. postDoc)

    return postHtml

  TextIO.writeFile (post & postId & unPostId & toHtmlFileName) postHtml & liftIO
