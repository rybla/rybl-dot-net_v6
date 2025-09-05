{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Service.Favicon where

import Blog.Paths
import Blog.Utility
import Control.Lens (Getter, makeLenses, to, (&), (^.))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import qualified Data.ByteString.Lazy as ByteString
import Data.Data (Proxy (..))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
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
    Proxy s -> URI -> HTTP.Manager -> m (Maybe FaviconInfo)

data FaviconInfo = FaviconInfo
  { _iconUri :: URI,
    _format :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''FaviconInfo

ident :: Getter FaviconInfo String
ident = iconUri . to \uri -> makeValidIdent (show uri)

filename :: Getter FaviconInfo FilePath
filename = ident

filepath :: Getter FaviconInfo FilePath
filepath = filename . to (offline.favicon.here </>)

internalIconUri :: Getter FaviconInfo URI
internalIconUri = to \info ->
  let relativeUri = URI.parseRelativeReference (info ^. filename) & fromJust
   in relativeUri `URI.relativeTo` online.favicon.here

cache ::
  forall s m.
  (FaviconService s, MonadIO m, MonadError Doc m) =>
  URI -> HTTP.Manager -> m (Maybe FaviconInfo)
cache uri manager = do
  let fpFaviconInfo = fromUriToFilePathOfFaviconInfo uri
  doesFileExist fpFaviconInfo & liftIO >>= \case
    True -> do
      mb_info <- do
        putStrLn ("reading file: " ++ fpFaviconInfo) & liftIO
        ByteString.readFile fpFaviconInfo & liftIO <&> decode @(Maybe FaviconInfo) >>= \case
          Nothing -> throwError @Doc $ "Failed to decode favicon info data file at" <+> text fpFaviconInfo
          Just mb_info -> return mb_info
      return mb_info
    False -> do
      mb_info <- fetchFaviconInfo (Proxy @s) uri manager
      case mb_info of
        Nothing -> return ()
        Just info -> do
          request <- HTTP.parseRequest (info ^. iconUri . to show) & liftIO
          response <- manager & HTTP.httpLbs request & liftIO
          ByteString.writeFile (info ^. filepath) (HTTP.responseBody response) & liftIO
      ByteString.writeFile fpFaviconInfo (encode mb_info) & liftIO
      return mb_info

baseFaviconInfo :: FaviconInfo
baseFaviconInfo =
  FaviconInfo
    { _iconUri = baseFaviconUri,
      _format = baseFaviconFormat
    }

missingFaviconInfo :: FaviconInfo
missingFaviconInfo =
  FaviconInfo
    { _iconUri = baseFaviconUri,
      _format = baseFaviconFormat
    }
