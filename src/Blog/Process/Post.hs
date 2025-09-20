{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Process.Post where

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
import Service.Preview (PreviewService)
import Text.Pandoc (Pandoc (..))
import Text.PrettyPrint.HughesPJClass (Doc)

processPost ::
  (FaviconService, PreviewService, MonadError Doc m, MonadState env m, MonadIO m) =>
  Lens' env Network.Manager ->
  Lens' env (Map URI [Link]) ->
  Lens' env (Map URI [Link]) ->
  Lens' env Post ->
  m ()
processPost manager outLinks inLinks post = do
  mgr <- gets (^. manager)
  ph <- gets (^. post . postHref)

  post . postDoc %=* commonTransformations

  (post . postDoc .=) =<< addLinkPreviews mgr =<< gets (^. post . postDoc)

  do
    ph <- gets (^. post . postHref)
    ols <- gets (^. outLinks . at ph . to (Maybe.fromMaybe []))
    post . postDoc %=* addReferencesSection ols

  do
    ils <- gets (^. inLinks . at ph . to (Maybe.fromMaybe []))
    post . postDoc %=* addCitationsSection ils

  whenM (gets (^. post . postTableOfContentsEnabled)) do
    post . postDoc %=* addTableOfContents

  postSnapshot <- gets (^. post)
  post . postDoc %=* addPostHeader postSnapshot

  post %=* addPostSignatureSection

  post . postDoc %=* addLinkFavicons mgr

  return ()

addPostHeader :: (Monad m) => Post -> Pandoc -> m Pandoc
addPostHeader post (Pandoc meta blocks) = do
  return $
    Pandoc meta $
      concat
        [ renderPostHeader post,
          blocks
        ]
