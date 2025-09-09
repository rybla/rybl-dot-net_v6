{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Service.Preview where

import qualified Blog.Config as Config
import qualified Blog.Paths as Paths
import Blog.Utility (logM, makeValidIdent)
import Control.Lens hiding (preview, (<.>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

class PreviewService s where
  previewUri ::
    (MonadIO m, MonadError Doc m) =>
    Proxy s -> URI -> HTTP.Manager -> m Preview

data Preview = Preview
  { _title :: String,
    _description :: String
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

cache ::
  forall s m.
  (PreviewService s, MonadIO m, MonadError Doc m) =>
  URI -> HTTP.Manager -> m Preview
cache uri manager = do
  logM "Preview.cache" $ "uri =" <+> text (show uri)
  if URI.uriIsRelative uri
    then do
      return basePreview
    else do
      let previewFilePath = Paths.offline.preview.here </> (uri & show & makeValidIdent & Paths.toDataFileName)
      doesFileExist previewFilePath & liftIO >>= \case
        True -> do
          ByteString.readFile previewFilePath & liftIO <&> Aeson.decode @Preview >>= \case
            Nothing -> throwError @Doc $ "Failed to decode preview data file at" <+> text previewFilePath
            Just preview -> return preview
        False -> do
          preview <- previewUri (Proxy @s) uri manager
          ByteString.writeFile previewFilePath (preview & Aeson.encode) & liftIO
          return preview

basePreview :: Preview
basePreview =
  Preview
    { _title = Config.baseTitle,
      _description = Config.baseDescription
    }
