{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Service.Favicon where

import Blog.Bug (MonadBug, throwBug)
import Blog.Paths
import Blog.Utility
import Control.Lens (makeLenses, (&), (^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as ByteString
import Data.Data (Proxy)
import qualified Network.HTTP.Client as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import System.FilePath ((<.>), (</>))
import Text.Pandoc (fileExists)
import Text.Pandoc.Class (PandocMonad)
import Text.PrettyPrint.HughesPJClass

class FaviconService s where
  fetchFaviconInfo ::
    (MonadIO m, MonadError Doc m) =>
    Proxy s -> URI -> HTTP.Manager -> m (Maybe FaviconInfo)

data FaviconInfo = FaviconInfo
  { _iconUri :: URI,
    _format :: String
  }
  deriving (Show)

makeLenses ''FaviconInfo

faviconIdent :: FaviconInfo -> String
faviconIdent info =
  info
    & (^. iconUri)
    & show
    & makeValidIdent

faviconFileName :: FaviconInfo -> String
faviconFileName info = faviconIdent info <.> info ^. format

faviconOfflineFilePath :: FaviconInfo -> FilePath
faviconOfflineFilePath info = offline.favicon.here </> faviconFileName info

faviconOnlineUri ::
  (MonadBug m) =>
  FaviconInfo -> m URI
faviconOnlineUri info = do
  identRelUri <-
    URI.parseRelativeReference (faviconFileName info)
      & maybe
        ( throwBug . vcat $
            [ "[faviconOnlineUri] Failed to parse favicon file name as a relative URI reference",
              "info =" <+> text (show info)
            ]
        )
        return
  return $ identRelUri `URI.relativeTo` online.favicon.here

cacheFavicon ::
  (MonadIO m, PandocMonad m) =>
  FaviconInfo -> HTTP.Manager -> m ()
cacheFavicon info manager = do
  -- check if already exists in cache
  let fp = faviconOfflineFilePath info
  fileExists fp >>= \case
    True -> do
      -- if it does, then do nothing
      return ()
    False -> do
      -- if doesn't, then download to cache
      let requestUrl = info ^. iconUri & show
      request <- HTTP.parseRequest requestUrl & liftIO
      response <- manager & HTTP.httpLbs request & liftIO
      ByteString.writeFile (faviconOfflineFilePath info) (HTTP.responseBody response) & liftIO
