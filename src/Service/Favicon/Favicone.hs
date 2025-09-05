module Service.Favicon.Favicone (FaviconeService) where

import Blog.Paths
import Blog.Utility
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import qualified Service.Favicon as Favicon
import Text.PrettyPrint.HughesPJClass

data FaviconeResponse = FaviconResponse
  { hasIcon :: Bool,
    icon :: String,
    format :: String
  }
  deriving (Show, Generic, FromJSON)

favicone :: (MonadIO m, MonadError Doc m) => URI -> HTTP.Manager -> m FaviconeResponse
favicone uri manager = do
  if URI.uriIsAbsolute uri
    then do
      let host = extractUriHost uri
      let requestUrl = "https://favicone.com/" ++ host ++ "?json"
      request <- HTTP.parseRequest requestUrl & liftIO
      response <- manager & HTTP.httpLbs request & liftIO
      decode (HTTP.responseBody response) & fromMaybe ("Failed to decode response:" <+> (text . show . HTTP.responseBody $ response))
    else
      return
        FaviconResponse
          { hasIcon = True,
            icon = show $ baseFaviconUri `URI.relativeTo` online.here,
            format = "ico"
          }

data FaviconeService

instance Favicon.FaviconService FaviconeService where
  fetchFaviconInfo _ uri manager = do
    response <- manager & favicone uri
    case response.hasIcon of
      False -> return Nothing
      True -> do
        iconUri <- URI.parseURI response.icon & fromMaybe ("Failed to parse icon URI:" <+> text response.icon)
        return . Just $
          Favicon.FaviconInfo
            { Favicon._iconUri = iconUri,
              Favicon._format = response.format
            }
