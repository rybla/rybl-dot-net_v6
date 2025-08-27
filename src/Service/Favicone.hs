{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Service.Favicone
  ( FaviconeResponse (..),
    fetchFavicone,
  )
where

import Blog.Paths (faviconUri)
import Blog.Utility
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Function ((&))
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI
import Text.PrettyPrint.HughesPJClass (render, text, (<+>))

data FaviconeResponse = FaviconResponse
  { hasIcon :: Bool,
    icon :: String,
    format :: String
  }
  deriving (Show, Generic, FromJSON)

fetchFavicone :: (MonadIO m, MonadError String m) => String -> m FaviconeResponse
fetchFavicone url = do
  uri <- parseURI url & maybe (throwError . render $ "Failed to parse URL:" <+> text (show url)) return
  if uriIsAbsolute uri
    then do
      manager <- newManager tlsManagerSettings & liftIO
      let host = extractHost uri
      let requestUrl = "https://favicone.com/" ++ host ++ "?json"
      liftIO . putStrLn $ "requestUrl = " ++ requestUrl
      request <- parseRequest requestUrl & liftIO
      response <- httpLbs request manager & liftIO
      decode (responseBody response)
        & maybe (throwError . render $ "Failed to decode response body as FaviconeResponse") return
    else
      return
        FaviconResponse
          { hasIcon = True,
            icon = faviconUri & show,
            format = "ico"
          }
