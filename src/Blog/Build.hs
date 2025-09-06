{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blog.Build (main) where

import Blog.Pandoc (fromDocError)
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Parse.Post as Parse.Post
import Blog.Paths as Paths
import Blog.Utility (fromEither)
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client as Network
import qualified Network.HTTP.Client.TLS as Network
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
  manager <- Network.newManager Network.tlsManagerSettings & liftIO

  parsePostEnv <- do
    let parsePostEnv = Parse.Post.newParsePostEnv manager
    (`execStateT` parsePostEnv) $
      listDirectory offline.post_markdown.here & liftIO >>= traverse_ \postFileName ->
        when (".md" `isSuffixOf` postFileName) do
          postText <- Text.readFile (offline.post_markdown.here </> postFileName) & liftIO
          templateText <- Text.readFile (offline.template.here </> "post.html") & liftIO

          postDoc <- Parse.Post.parsePost @FaviconeService postText

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

            postTemplate <- Pandoc.compileTemplate offline.template.here templateText & liftIO >>= fromEither (("Error when parsing template:" <+>) . text) & fromDocError

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

          Text.writeFile
            (offline.post.here </> (postFileName `replaceExtension` ".html"))
            postHtml
            & liftIO

          return ()

  let _ = undefined parsePostEnv -- TODO: use parsePostEnv to build graph
  --
  return ()
