{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Paths where

import Blog.Common
import qualified Blog.Config as Config
import Blog.PathsTh (MakeRootParams (..), makeRoot)
import Blog.Utility (fromMaybe, logM)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Function ((&))
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Tree (Tree (..))
import Network.URI (URI)
import qualified Network.URI as URI
import System.FilePath ((<.>), (</>))
import Text.Pandoc (Pandoc)
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

baseUri :: URI
baseUri = case Config.mode of
  Config.Production -> "https://rybl.net" & Maybe.fromJust . URI.parseURI
  Config.Development -> "http://localhost:8080" & Maybe.fromJust . URI.parseURI

baseFaviconUri :: URI
baseFaviconUri = relUri `URI.relativeTo` baseUri
  where
    relUri = "favicon.ico" & URI.parseRelativeReference & Maybe.fromJust

baseFaviconFormat :: String
baseFaviconFormat = "ico"

missingFaviconUri :: URI
missingFaviconUri = relUri `URI.relativeTo` baseUri
  where
    relUri = "missing.ico" & URI.parseRelativeReference & Maybe.fromJust

missingFaviconFormat :: String
missingFaviconFormat = "ico"

makeRoot
  [ MakeRootParams
      { valName = "offline",
        nodeType = [t|String|],
        toNode = [|("asset" </>)|]
      },
    MakeRootParams
      { valName = "online",
        nodeType = [t|URI|],
        toNode =
          [|
            \s ->
              let relUri = s & URI.parseRelativeReference & Maybe.fromJust
               in relUri `URI.relativeTo` baseUri
            |]
      }
  ]
  [ Node "post_markdown" [],
    Node "post_data" [],
    Node "post" [],
    Node "favicon" [],
    Node "template" []
  ]

readPostMarkdown :: (MonadIO m) => PostId -> m Text
readPostMarkdown postId = do
  logM $ "readPostMarkdown:" <+> text fp
  Text.readFile fp & liftIO
  where
    fp = offline.post_markdown.here </> (postId & unPostId & toMarkdownFileName)

writePostData :: (MonadIO m) => PostId -> Pandoc -> m ()
writePostData postId doc = liftIO do
  logM $ "writePostData:" <+> text fp
  ByteString.writeFile fp (Aeson.encode doc)
  where
    fp = offline.post_data.here </> (postId & unPostId & toDataFileName)

readPostData :: (MonadIO m, MonadError Doc m) => PostId -> m Pandoc
readPostData postId = do
  logM $ "readPostData:" <+> text fp
  ByteString.readFile fp
    & liftIO
    >>= return . Aeson.decode
    >>= fromMaybe ("Failed to parse post data from file:" <+> text fp)
  where
    fp = offline.post_data.here </> (postId & unPostId & toDataFileName)

readTemplateHtml :: (MonadIO m) => FilePath -> m Text
readTemplateHtml templateId = do
  logM $ "readTemplateHtml:" <+> text fp
  Text.readFile fp & liftIO
  where
    fp = offline.template.here </> (templateId & toHtmlFileName)

writePostHtml :: (MonadIO m) => PostId -> Text -> m ()
writePostHtml postId htmlText = do
  logM $ "writePostHtml:" <+> text fp
  Text.writeFile fp htmlText & liftIO
  where
    fp = offline.post.here </> (postId & unPostId & toHtmlFileName)

toMarkdownFileName :: FilePath -> FilePath
toMarkdownFileName = (<.> ".md")

toDataFileName :: FilePath -> FilePath
toDataFileName = (<.> ".json")

toHtmlFileName :: FilePath -> FilePath
toHtmlFileName = (<.> ".html")
