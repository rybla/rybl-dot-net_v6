{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Service.Favicon where

import Blog.Paths
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
import System.FilePath ((<.>), (</>))
import Text.PrettyPrint.HughesPJClass

fromUriToFilePathOfFaviconInfo :: URI -> FilePath
fromUriToFilePathOfFaviconInfo uri = offline.favicon.here </> makeValidIdent (show uri) <.> "json"

identOfUri :: URI -> String
identOfUri = makeValidIdent . show

class FaviconService s where
  fetchFaviconInfo ::
    (MonadIO m, MonadError Doc m) =>
    Proxy s -> URI -> HTTP.Manager -> m FaviconInfo

data FaviconInfo = FaviconInfo
  { _originalIconRef :: URI,
    _mirrorIconRef :: URI,
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
  logM $ "URI.uriIsRelative" <+> pPrint (show uri) <+> "=" <+> pPrint (URI.uriIsRelative uri)
  if URI.uriIsRelative uri
    then do
      return baseFaviconInfo
    else do
      let fpFaviconInfo = fromUriToFilePathOfFaviconInfo uri
      doesFileExist fpFaviconInfo & liftIO >>= \case
        True -> do
          putStrLn ("reading favicone data file: " ++ fpFaviconInfo) & liftIO
          ByteString.readFile fpFaviconInfo
            & liftIO
            <&> decode @(Maybe FaviconInfo)
            >>= \case
              Nothing -> throwError @Doc $ "Failed to decode favicon info data file at" <+> text fpFaviconInfo
              Just Nothing -> return missingFaviconInfo
              Just (Just info) -> return info
        False -> do
          logM "here1"
          !info <- fetchFaviconInfo (Proxy @s) uri manager
          logM "here2"
          when (not (URI.uriIsRelative (info ^. originalIconRef))) do
            !request <- HTTP.parseRequest (info ^. originalIconRef . to show) & liftIO
            !response <- manager & HTTP.httpLbs request & liftIO
            ByteString.writeFile (info ^. mirrorIconFilePath) (HTTP.responseBody response) & liftIO
            ByteString.writeFile fpFaviconInfo (encode info) & liftIO
          return info

baseFaviconInfo :: FaviconInfo
baseFaviconInfo =
  FaviconInfo
    { _originalIconRef = baseFaviconUri,
      _mirrorIconRef = baseFaviconUri,
      _mirrorIconFilePath = baseFaviconFilePath,
      _format = baseFaviconFormat
    }

missingFaviconInfo :: FaviconInfo
missingFaviconInfo =
  FaviconInfo
    { _originalIconRef = missingFaviconUri,
      _mirrorIconRef = missingFaviconUri,
      _mirrorIconFilePath = missingFaviconFilePath,
      _format = missingFaviconFormat
    }
