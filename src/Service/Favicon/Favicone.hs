{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Service.Favicon.Favicone () where

import Blog.Common (UriReference (..))
import qualified Blog.Paths as Paths
import Blog.Utility
import Control.Lens hiding ((<.>))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.URI (URI)
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
  let requestUrl = "https://favicone.com/" ++ host ++ "?json=true"
  logM "favicone" $ "defining request for favicone; requestUrl =" <+> text requestUrl
  request <- HTTP.parseRequest requestUrl & liftIO
  logM "favicone" "sending request to favicone"
  response <- manager & HTTP.httpLbs request & liftIO
  logM "favicone" "got back response from favicone"
  response' <- decode (HTTP.responseBody response) & fromMaybe ("Failed to decode response:" <+> (text . show . HTTP.responseBody $ response))
  logM "favicone" $ "decoded response from favicone:" <+> showDoc response'
  return response'

instance Favicon.FaviconService where
  fetchFaviconInfo uri manager = do
    response <- manager & favicone uri
    case response.hasIcon of
      False -> return Favicon.missingFaviconInfo
      True -> do
        let ident = uri & show & makeValidIdent
        let faviconFileName = ident <.> response.format
        iconUri <- parseUriM response.icon
        mirrorIconRef <- Paths.online.favicon.here </> faviconFileName & parseUriReferenceM
        let mirrorIconFilePath = Paths.offline.favicon.here </> faviconFileName
        return
          Favicon.FaviconInfo
            { Favicon.originalIconRef = iconUri & UriReference,
              Favicon.mirrorIconRef = mirrorIconRef & UriReference,
              Favicon.mirrorIconFilePath = mirrorIconFilePath,
              Favicon.format = response.format
            }
