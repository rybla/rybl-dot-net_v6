{-# LANGUAGE BangPatterns #-}

module Service.Favicon.Favicone (FaviconeService) where

import qualified Blog.Paths as Paths
import Blog.Utility
import Control.Category ((>>>))
import Control.Lens hiding ((<.>))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import qualified Service.Favicon as Favicon
import System.FilePath ((<.>), (</>))
import Text.PrettyPrint.HughesPJClass

data FaviconeResponse = FaviconResponse
  { hasIcon :: Bool,
    icon :: String,
    format :: String
  }
  deriving (Show, Generic, FromJSON)

favicone :: (MonadIO m, MonadError Doc m) => URI -> HTTP.Manager -> m FaviconeResponse
favicone uri manager = do
  let host = extractUriHost uri
  let requestUrl = "https://favicone.com/" ++ host ++ "?json"
  logM "defining request for favicone"
  request <- HTTP.parseRequest requestUrl & liftIO
  logM "sending request to favicone"
  !response <- manager & HTTP.httpLbs request & liftIO
  logM "got back response from favicone"
  !response' <- decode (HTTP.responseBody response) & fromMaybe ("Failed to decode response:" <+> (text . show . HTTP.responseBody $ response))
  logM $ "decoded response from favicone:" <+> showDoc response'
  return response'

data FaviconeService

instance Favicon.FaviconService FaviconeService where
  fetchFaviconInfo _ uri manager = do
    response <- manager & favicone uri
    logM "got response from favicone service"
    case response.hasIcon of
      False -> return Favicon.missingFaviconInfo
      True -> do
        iconUri <- parseUriM response.icon
        ident <-
          iconUri
            & URI.uriAuthority
            & fromMaybe ("URI should be absolute and therefor have an authority, but it doesn't:" <+> text (show iconUri))
            <&> (URI.uriRegName >>> makeValidIdent)
        let faviconPath = "" </> ident <.> response.format
        faviconPathUri <- parseUriReferenceM faviconPath
        return
          Favicon.FaviconInfo
            { Favicon._originalIconRef = iconUri,
              Favicon._mirrorIconRef = faviconPathUri `URI.relativeTo` Paths.online.favicon.here,
              Favicon._mirrorIconFilePath = Paths.offline.favicon.here </> faviconPath,
              Favicon._format = response.format
            }
