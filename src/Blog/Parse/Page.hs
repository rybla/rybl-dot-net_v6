{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Parse.Page where

import Blog.Common
import Blog.Pandoc (runPandocM)
import qualified Blog.Pandoc as Pandoc
import Blog.Parse.Common
import Blog.Utility
import Control.Lens
import Control.Monad (void)
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadIO)
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI (URI)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, pPrint, (<+>))

parsePage ::
  (MonadIO m, MonadError Doc m, MonadState env m) =>
  Lens' env (Map URI Text) ->
  Lens' env (Map URI [Link]) ->
  Lens' env (Map URI [Link]) ->
  PageId ->
  Text ->
  m Page
parsePage uriLabels outLinks inLinks pageId pageText = do
  logM "parsePost" $ "pageId =" <+> pPrint pageId
  doc <-
    pageText
      & Pandoc.readMarkdown
        commonReaderOptions
      & runPandocM

  pageHref <- toPageHref pageId
  pageTitle <- doc & Pandoc.getMetaValueSuchThat Pandoc.fromMetaString "title"
  uriLabels . at pageHref %= \case
    Just existingLabel -> error $ "attempted to add uriLabel for page " ++ show pageTitle ++ " at href " ++ show pageHref ++ " a second time; the existing label is " ++ show existingLabel
    Nothing -> Just pageTitle
  pageReferencesEnabled <-
    doc
      & Pandoc.getMetaValueMaybeSuchThat Pandoc.fromMetaBool "references"
      <&> Maybe.fromMaybe True
  pageTableOfContentsEnabled <-
    doc
      & Pandoc.getMetaValueMaybeSuchThat Pandoc.fromMetaBool "table_of_contents"
      <&> Maybe.fromMaybe True
  pageHeaderEnabled <-
    doc
      & Pandoc.getMetaValueMaybeSuchThat Pandoc.fromMetaBool "header"
      <&> Maybe.fromMaybe True

  void $
    doc & Pandoc.walkM \(x :: Pandoc.Inline) -> case x of
      Pandoc.Link _attr kids (refText, _) -> do
        ref <- refText & Text.unpack & parseUriReferenceM
        let outLink = Link kids ref
        outLinks . at pageHref %= maybe (Just [outLink]) (Just . (outLink :))
        let inLink = Link kids pageHref
        inLinks . at ref %= maybe (Just [inLink]) (Just . (inLink :))
        return x
      Pandoc.Image _attr kids (refText, _) -> do
        ref <- refText & Text.unpack & parseUriReferenceM
        let outLink = Link kids ref
        outLinks . at pageHref %= maybe (Just [outLink]) (Just . (outLink :))
        let inLink = Link kids pageHref
        inLinks . at ref %= maybe (Just [inLink]) (Just . (inLink :))
        return x
      _ -> return x

  return
    Page
      { _pageId = pageId,
        _pageHref = pageHref,
        _pageTitle = pageTitle,
        _pageReferencesEnabled = pageReferencesEnabled,
        _pageTableOfContentsEnabled = pageTableOfContentsEnabled,
        _pageHeaderEnabled = pageHeaderEnabled,
        _pageDoc = doc
      }
