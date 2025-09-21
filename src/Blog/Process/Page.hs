{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Process.Page where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
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
import Text.Pandoc (Pandoc (..))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, (<+>))

processPage ::
  (FaviconService, MonadError Doc m, MonadState env m, MonadIO m) =>
  Lens' env Network.Manager ->
  Lens' env (Map URI [Link]) ->
  Lens' env (Map URI [Link]) ->
  Lens' env Page ->
  m ()
processPage manager outLinks inLinks page = do
  gets (^. page) >>= \page' ->
    logM "processPost" $ "title =" <+> textDoc page'._pageTitle

  mgr <- gets (^. manager)
  ph <- gets (^. page . pageHref)

  page . pageDoc %=* commonTransformations

  whenM (gets (^. page . pageReferencesEnabled)) do
    ph <- gets (^. page . pageHref)
    ols <- gets (^. outLinks . at ph . to (Maybe.fromMaybe []))
    page . pageDoc %=* addReferencesSection ols

  do
    ils <- gets (^. inLinks . at ph . to (Maybe.fromMaybe []))
    page . pageDoc %=* addCitationsSection ils

  whenM (gets (^. page . pageTableOfContentsEnabled)) do
    page . pageDoc %=* addTableOfContents False

  pageSnapshot <- gets (^. page)
  page . pageDoc %=* addPageHeader pageSnapshot

  page . pageDoc %=* addLinkFavicons mgr

  return ()

addPageHeader :: (Monad m) => Page -> Pandoc -> m Pandoc
addPageHeader page (Pandoc meta blocks) = do
  return $
    Pandoc meta $
      concat
        [ renderPageHeader page,
          blocks
        ]

renderPageHeader :: Page -> [Pandoc.Block]
renderPageHeader page =
  concat
    [ [ Pandoc.Header
          2
          mempty
          [ Pandoc.Link
              (mempty & Pandoc.attrClasses %~ (["no-link-favicon", "no-link-preview"] ++))
              [Pandoc.Str page._pageTitle]
              (showText page._pageHref, mempty)
          ]
      ]
    ]
