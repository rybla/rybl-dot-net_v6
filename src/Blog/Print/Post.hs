{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printPost :: (MonadIO m, MonadError Doc m) => PostId -> m ()
printPost postId = do
  postDoc <- Paths.readPostData postId
  templateText <- Paths.readTemplateHtml "post"

  postTitle <- postDoc & Pandoc.getMetaValue "title" <&> Pandoc.stringify
  postTags <- postDoc & Pandoc.getMetaValueList "tags" <&> (<&> Pandoc.stringify)

  postHtml <- Pandoc.runPandocM do
    contentHtml <-
      Pandoc.writeHtml5String
        Pandoc.def
          { Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
          }
        postDoc

    let varsJson :: Aeson.Value
        varsJson =
          Aeson.object
            [ ("stylesheetHref", "post.css"),
              ("title", postTitle & Aeson.toJSON),
              ("tags", postTags & Aeson.toJSON),
              ("content", contentHtml & Aeson.toJSON)
            ]

    -- postTemplate <- Pandoc.compileTemplate Paths.offline.template.here templateText & liftIO >>= fromEither (("Error when parsing template:" <+>) . text) & fromDocError
    -- let _ = Pandoc.compileTemplate Paths.offline.template.here templateText
    --           & Pandoc.runWithParials

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
        postDoc

    return postHtml

  Paths.writePostHtml postId postHtml
