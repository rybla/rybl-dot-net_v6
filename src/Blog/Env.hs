{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Env where

import Blog.Common
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Network.HTTP.Client as Network
import qualified Network.HTTP.Client.TLS as Network
import Network.URI (URI)

data Env = Env
  { _uriLabels :: Map URI Text,
    _outLinks :: Map URI [Link],
    _inLinks :: Map URI [Link],
    _manager :: Network.Manager
  }

newEnv :: (MonadIO m) => m Env
newEnv = do
  manager <- Network.newManager Network.tlsManagerSettings & liftIO
  return
    Env
      { _uriLabels = Map.empty,
        _outLinks = Map.empty,
        _inLinks = Map.empty,
        _manager = manager
      }

makeLenses ''Env
