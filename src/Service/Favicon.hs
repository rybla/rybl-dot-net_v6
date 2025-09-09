{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Service.Favicon where

import Blog.Common
import qualified Blog.Paths as Paths
import Blog.Utility
import Control.Lens (makeLenses, to, (&), (^.))
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import qualified Data.ByteString.Lazy as ByteString
import Data.Data (Proxy (..))
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.PrettyPrint.HughesPJClass

class FaviconService s where
  fetchFaviconInfo ::
    (MonadIO m, MonadError Doc m) =>
    Proxy s -> URI -> HTTP.Manager -> m FaviconInfo

data FaviconInfo = FaviconInfo
  { _originalIconRef :: UriReference,
    _mirrorIconRef :: UriReference,
    _mirrorIconFilePath :: FilePath,
    _format :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''FaviconInfo

cache ::
  forall s m.
  (FaviconService s, MonadIO m, MonadError Doc m) =>
  URI -> HTTP.Manager -> m FaviconInfo
cache uri manager = do
  logM "Favicone.cache" $ "uri =" <+> text (show uri)
  if URI.uriIsRelative uri
    then do
      return baseFaviconInfo
    else do
      let infoFilePath = Paths.offline.favicon.here </> (uri & show & makeValidIdent & Paths.toDataFileName)
      doesFileExist infoFilePath & liftIO >>= \case
        True -> do
          ByteString.readFile infoFilePath
            & liftIO
            <&> decode @FaviconInfo
            >>= \case
              Nothing -> throwError @Doc $ "Failed to decode favicon info data file at" <+> text infoFilePath
              Just info -> return info
        False -> do
          info <- fetchFaviconInfo (Proxy @s) uri manager
          when (not (URI.uriIsRelative (info ^. originalIconRef . unUriReference))) do
            request <- HTTP.parseRequest (info ^. originalIconRef . unUriReference . to show) & liftIO
            response <- manager & HTTP.httpLbs request & liftIO
            ByteString.writeFile (info ^. mirrorIconFilePath) (HTTP.responseBody response) & liftIO
            ByteString.writeFile infoFilePath (encode info) & liftIO
          return info

baseFaviconInfo :: FaviconInfo
baseFaviconInfo =
  FaviconInfo
    { _originalIconRef = Paths.baseFaviconUri & UriReference,
      _mirrorIconRef = Paths.baseFaviconUri & UriReference,
      _mirrorIconFilePath = Paths.baseFaviconFilePath,
      _format = Paths.baseFaviconFormat
    }

missingFaviconInfo :: FaviconInfo
missingFaviconInfo =
  FaviconInfo
    { _originalIconRef = Paths.missingFaviconUri & UriReference,
      _mirrorIconRef = Paths.missingFaviconUri & UriReference,
      _mirrorIconFilePath = Paths.missingFaviconFilePath,
      _format = Paths.missingFaviconFormat
    }
