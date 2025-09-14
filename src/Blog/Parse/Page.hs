{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Parse.Page where

import Blog.Common
import Blog.Pandoc (runPandocM)
import qualified Blog.Pandoc as Pandoc
import Blog.Utility
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.Writer (MonadIO)
import Data.Text (Text)
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, pPrint, (<+>))

parsePage ::
  (MonadIO m, MonadError Doc m) =>
  PageId ->
  Text ->
  m Page
parsePage pageId pageText = do
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

  return
    Page
      { _pageId = pageId,
        _pageHref = pageHref,
        _pageTitle = pageTitle,
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
          -- attributes
          Pandoc.Ext_attributes,
          Pandoc.Ext_fenced_code_attributes,
          Pandoc.Ext_header_attributes,
          Pandoc.Ext_inline_code_attributes,
          Pandoc.Ext_link_attributes,
          Pandoc.Ext_mmd_link_attributes,
          Pandoc.Ext_raw_attribute
        ]
