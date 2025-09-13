{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Service.Preview where

import Blog.Common
import qualified Blog.Config as Config
import Blog.Utility
import Control.Lens hiding (preview, (<.>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import System.Directory (doesFileExist)
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

class PreviewService where
  previewUri ::
    (MonadIO m, MonadError Doc m) =>
    URI -> HTTP.Manager -> m Preview

data Preview = Preview
  { title :: String,
    description :: String
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

cache ::
  forall m.
  (PreviewService, MonadIO m, MonadError Doc m) =>
  URI -> HTTP.Manager -> m Preview
cache uri manager = do
  logM "Preview.cache" $ "uri =" <+> text (show uri)
  if URI.uriIsRelative uri
    then do
      return basePreview
    else do
      let previewFilePath = uri & toFaviconInfoFilePath
      doesFileExist previewFilePath & liftIO >>= \case
        True -> do
          ByteString.readFile previewFilePath & liftIO <&> Aeson.decode @Preview >>= \case
            Nothing -> throwError @Doc $ "Failed to decode preview data file at" <+> text previewFilePath
            Just preview -> return preview
        False -> do
          preview <- previewUri uri manager
          ByteString.writeFile previewFilePath (preview & Aeson.encode) & liftIO
          return preview

basePreview :: Preview
basePreview =
  Preview
    { title = Config.baseTitle,
      description = Config.baseDescription
    }
