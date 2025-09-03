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
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Exts (fromList)
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, (</>))
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, render, text, (<+>))

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> throwError . userError . render $ "Error:" <+> text (show err)
    Right it -> return it

main' :: ExceptT Doc IO ()
main' = do
  listDirectory offline.post_markdown.here & lift >>= traverse_ \postFileName ->
    when (".md" `isSuffixOf` postFileName) do
      postText <- Text.readFile (offline.post_markdown.here </> postFileName) & lift
      templateText <- Text.readFile (offline.template.here </> "post.html") & lift
      result <- Pandoc.runPandocM do
        doc <- parsePost postText
        title <- doc & Pandoc.getMetaValue "title" <&> Pandoc.stringify
        tags <- doc & Pandoc.getMetaValueList "tags" <&> (<&> Pandoc.stringify)

        postHtml <- Pandoc.writeHtml5String Pandoc.def doc

        let varsJson :: Aeson.Value
            varsJson =
              Aeson.object
                [ ("stylesheetHref", "styles.css"),
                  ("title", title & Aeson.toJSON),
                  ("tags", tags & Aeson.toJSON),
                  ("content", postHtml & Aeson.toJSON)
                ]
        vars <- case Aeson.parseEither Aeson.parseJSON varsJson of
          Left err -> throwError . Pandoc.PandocTemplateError . Text.pack . ("Error when parsing template variables JSON: " <>) $ err
          Right a -> return a

        template <-
          Pandoc.compileTemplate offline.template.here templateText
            & runIdentity
            & either (throwError . Pandoc.PandocTemplateError . Text.pack . ("Error when parsing template: " <>)) return

        html <-
          Pandoc.writeHtml5String
            Pandoc.def
              { Pandoc.writerHTMLMathMethod = Pandoc.PlainMath,
                Pandoc.writerTemplate = Just template,
                Pandoc.writerVariables = vars
              }
            doc
        return html
      Text.writeFile
        (offline.post_html.here </> (postFileName `replaceExtension` ".html"))
        result
        & lift
      return ()
  return ()
