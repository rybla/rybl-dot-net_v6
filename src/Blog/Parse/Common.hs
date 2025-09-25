{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Parse.Common where

import Text.Pandoc as Pandoc

commonReaderOptions :: Pandoc.ReaderOptions
commonReaderOptions =
  def
    { Pandoc.readerStandalone = True,
      Pandoc.readerExtensions =
        Pandoc.extensionsFromList
          [ -- metadata
            Pandoc.Ext_yaml_metadata_block,
            -- styles
            Pandoc.Ext_mark,
            Pandoc.Ext_strikeout,
            Pandoc.Ext_subscript,
            Pandoc.Ext_superscript,
            -- groupings
            Pandoc.Ext_tex_math_dollars,
            Pandoc.Ext_backtick_code_blocks,
            Pandoc.Ext_bracketed_spans,
            Pandoc.Ext_fenced_divs,
            Pandoc.Ext_pipe_tables,
            Pandoc.Ext_inline_notes,
            Pandoc.Ext_footnotes,
            Pandoc.Ext_raw_html,
            -- attributes
            Pandoc.Ext_attributes,
            Pandoc.Ext_fenced_code_attributes,
            Pandoc.Ext_header_attributes,
            Pandoc.Ext_inline_code_attributes,
            Pandoc.Ext_link_attributes,
            Pandoc.Ext_mmd_link_attributes,
            Pandoc.Ext_raw_attribute
          ]
    }
