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
  Lens' env (Map URI [Link]) ->
  Lens' env (Map URI [Link]) ->
  PageId ->
  Text ->
  m Page
parsePage outLinks inLinks pageId pageText = do
  logM "parsePost" $ "pageId =" <+> pPrint pageId
  doc <-
    pageText
      & Pandoc.readMarkdown
        Pandoc.def
          { Pandoc.readerStandalone = True,
            Pandoc.readerExtensions
          }
      & runPandocM

  pageHref <- toPageHref pageId
  pageTitle <- doc & Pandoc.getMetaValueSuchThat Pandoc.fromMetaString "title"
  pageReferencesEnabled <-
    doc
      & Pandoc.getMetaValueMaybeSuchThat Pandoc.fromMetaBool "references"
      <&> Maybe.fromMaybe True
  pageTableOfContentsEnabled <-
    doc
      & Pandoc.getMetaValueMaybeSuchThat Pandoc.fromMetaBool "table_of_contents"
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
        _pageDoc = doc
      }
  where
    readerExtensions =
      Pandoc.extensionsFromList
        [ -- metadata
          Pandoc.Ext_yaml_metadata_block,
          -- styles
          Pandoc.Ext_mark,
          Pandoc.Ext_strikeout,
          Pandoc.Ext_subscript,
          Pandoc.Ext_superscript,
          -- Pandoc.Ext_footnotes,
          -- groupings
          Pandoc.Ext_tex_math_dollars,
          Pandoc.Ext_backtick_code_blocks,
          Pandoc.Ext_bracketed_spans,
          Pandoc.Ext_fenced_divs,
          Pandoc.Ext_pipe_tables,
          -- attributes
          Pandoc.Ext_attributes,
          Pandoc.Ext_fenced_code_attributes,
          Pandoc.Ext_header_attributes,
          Pandoc.Ext_inline_code_attributes,
          Pandoc.Ext_link_attributes,
          Pandoc.Ext_mmd_link_attributes,
          Pandoc.Ext_raw_attribute
        ]
