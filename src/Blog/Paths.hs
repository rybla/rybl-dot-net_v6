{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Paths where

import Blog.Common
import Blog.PathsTh (MakeRootParams (..), makeRoot)
import Blog.Utility (fromMaybe, logM)
import Control.Lens hiding ((<.>))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Tree (Tree (..))
import Network.URI (URI)
import Network.URI.Static (relativeReference)
import System.FilePath ((<.>), (</>))
import Text.Pandoc (Pandoc)
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

baseFaviconUri :: URI
baseFaviconUri = [relativeReference|/favicon.ico|]

baseFaviconFilePath :: FilePath
baseFaviconFilePath = "/favicon.ico"

baseFaviconFormat :: String
baseFaviconFormat = "ico"

missingFaviconUri :: URI
missingFaviconUri = [relativeReference|/missing.ico|]

missingFaviconFilePath :: FilePath
missingFaviconFilePath = "/missing.ico"

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
        nodeType = [t|String|],
        toNode = [|("/" </>)|]
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
  logM "readPostMarkdown" $ "fp =" <+> text fp
  Text.readFile fp & liftIO
  where
    fp = offline.post_markdown.here </> (postId ^. unPostId . to toMarkdownFileName)

writePostData :: (MonadIO m) => PostId -> Pandoc -> m ()
writePostData postId doc = liftIO do
  logM "writePostData" $ "fp =" <+> text fp
  ByteString.writeFile fp (Aeson.encode doc)
  where
    fp = offline.post_data.here </> (postId ^. unPostId . to toDataFileName)

readPostData :: (MonadIO m, MonadError Doc m) => PostId -> m Pandoc
readPostData postId = do
  logM "readPostData" $ "fp =" <+> text fp
  ByteString.readFile fp
    & liftIO
    >>= return . Aeson.decode
    >>= fromMaybe ("Failed to parse post data from file:" <+> text fp)
  where
    fp = offline.post_data.here </> (postId ^. unPostId . to toDataFileName)

readTemplateHtml :: (MonadIO m) => FilePath -> m Text
readTemplateHtml templateId = do
  logM "readTemplateHtml" $ "fp =" <+> text fp
  Text.readFile fp & liftIO
  where
    fp = offline.template.here </> (templateId & toHtmlFileName)

writePostHtml :: (MonadIO m) => PostId -> Text -> m ()
writePostHtml postId htmlText = do
  logM "writePostHtml" $ "fp =" <+> text fp
  Text.writeFile fp htmlText & liftIO
  where
    fp = offline.post.here </> (postId ^. unPostId . to toHtmlFileName)

toMarkdownFileName :: FilePath -> FilePath
toMarkdownFileName = (<.> ".md")

toDataFileName :: FilePath -> FilePath
toDataFileName = (<.> ".json")

toHtmlFileName :: FilePath -> FilePath
toHtmlFileName = (<.> ".html")
