{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Process.Page where

import Blog.Common
import Blog.Process.Common
import Blog.Utility
import Control.Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State (MonadIO, MonadState, gets)
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Client as Network
import Network.URI (URI)
import Service.Favicon (FaviconService)
import Text.PrettyPrint.HughesPJClass (Doc)

processPage ::
  (FaviconService, MonadError Doc m, MonadState env m, MonadIO m) =>
  Lens' env Network.Manager ->
  Lens' env (Map URI [Link]) ->
  Lens' env (Map URI [Link]) ->
  Lens' env Page ->
  m ()
processPage manager outLinks inLinks page = do
  mgr <- gets (^. manager)
  ph <- gets (^. page . pageHref)

  page . pageDoc %= removeCommentBlocks

  do
    ph <- gets (^. page . pageHref)
    ols <- gets (^. outLinks . at ph . to (Maybe.fromMaybe []))
    page . pageDoc .=* addReferencesSection ols

  do
    ils <- gets (^. inLinks . at ph . to (Maybe.fromMaybe []))
    page . pageDoc .=* addCitationsSection ils

  page . pageDoc .=* addLinkFavicons mgr

  return ()
