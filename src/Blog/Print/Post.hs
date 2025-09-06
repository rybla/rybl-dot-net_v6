{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Print.Post where

import Blog.Common
import Blog.Pandoc (fromDocError)
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Utility (fromEither)
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printPost :: (MonadIO m, MonadError Doc m) => PostId -> m ()
printPost postId = do
  postDoc <- Paths.readPostData postId
  templateText <- Paths.readTemplateHtml "post" -- Text.readFile (offline.template.here </> "post.html") & liftIO
  postTitle <- postDoc & Pandoc.getMetaValue "title" <&> Pandoc.stringify
  postTags <- postDoc & Pandoc.getMetaValueList "tags" <&> (<&> Pandoc.stringify)

  postHtml <- Pandoc.runPandocM do
    contentHtml <- Pandoc.writeHtml5String Pandoc.def postDoc

    let varsJson :: Aeson.Value
        varsJson =
          Aeson.object
            [ ("stylesheetHref", "post.css"),
              ("title", postTitle & Aeson.toJSON),
              ("tags", postTags & Aeson.toJSON),
              ("content", contentHtml & Aeson.toJSON)
            ]

    postTemplate <- Pandoc.compileTemplate Paths.offline.template.here templateText & liftIO >>= fromEither (("Error when parsing template:" <+>) . text) & fromDocError

    postHtml <- do
      vars <- Aeson.parseEither Aeson.parseJSON varsJson & fromEither (("Error when parsing template variables JSON:" <+>) . text) & fromDocError
      Pandoc.writeHtml5String
        Pandoc.def
          { Pandoc.writerHTMLMathMethod = Pandoc.PlainMath,
            Pandoc.writerTemplate = Just postTemplate,
            Pandoc.writerVariables = vars
          }
        postDoc

    return postHtml

  Paths.writePostHtml postId postHtml
