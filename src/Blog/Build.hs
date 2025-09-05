{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blog.Build (main) where

import qualified Blog.Pandoc as Pandoc
import Blog.Parse (parsePost)
import Blog.Paths as Paths
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Service.Favicon.Favicone (FaviconeService)
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, (</>))
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import Text.PrettyPrint.HughesPJClass

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error . render $ "Error:" <+> text (show err)
    Right it -> return it

main' :: ExceptT Doc IO ()
main' = do
  listDirectory offline.post_markdown.here & lift >>= traverse_ \postFileName ->
    when (".md" `isSuffixOf` postFileName) do
      postText <- Text.readFile (offline.post_markdown.here </> postFileName) & lift
      templateText <- Text.readFile (offline.template.here </> "post.html") & lift

      result <- Pandoc.runPandocM do
        postDoc <- parsePost @FaviconeService postText
        postTitle <- postDoc & Pandoc.getMetaValue "title" <&> Pandoc.stringify
        postTags <- postDoc & Pandoc.getMetaValueList "tags" <&> (<&> Pandoc.stringify)

        contentHtml <- Pandoc.writeHtml5String Pandoc.def postDoc

        let varsJson :: Aeson.Value
            varsJson =
              Aeson.object
                [ ("stylesheetHref", "styles.css"),
                  ("title", postTitle & Aeson.toJSON),
                  ("tags", postTags & Aeson.toJSON),
                  ("content", contentHtml & Aeson.toJSON)
                ]

        postTemplate <- Pandoc.compileTemplate offline.template.here templateText & runIdentity & either (throwError . Pandoc.PandocTemplateError . Text.pack . ("Error when parsing template: " ++)) return
        postHtml <- do
          vars <- Aeson.parseEither Aeson.parseJSON varsJson & either (\err -> throwError . Pandoc.PandocTemplateError . Text.pack . ("Error when parsing template variables JSON: " ++) $ err) return
          Pandoc.writeHtml5String
            Pandoc.def
              { Pandoc.writerHTMLMathMethod = Pandoc.PlainMath,
                Pandoc.writerTemplate = Just postTemplate,
                Pandoc.writerVariables = vars
              }
            postDoc
        return postHtml

      Text.writeFile
        (offline.post_html.here </> (postFileName `replaceExtension` ".html"))
        result
        & lift
      return ()
  return ()
