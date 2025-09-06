{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Parse.Post where

import Blog.Pandoc (runPandocM)
import qualified Blog.Paths as Paths
import Blog.Utility (logM)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Except (MonadError)
import Control.Monad.Writer (MonadIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Network.URI (URI)
import Text.Pandoc (Pandoc (..))
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, (<+>), pPrint)
import Blog.Common

data Link
  = InternalLink String
  | ExternalLink URI

data Env = Env
  { _outLinks :: Map String [Link],
    _inLinks :: Map String [Link]
  }

newEnv :: Env
newEnv = do
  Env
    { _outLinks = Map.empty,
      _inLinks = Map.empty
    }

makeLenses ''Env

parsePost ::
  forall m.
  (MonadIO m, MonadError Doc m) =>
  PostId -> m Pandoc
parsePost postId = do
  logM $ "parsePost: " <+> pPrint postId
  txt <- Paths.readPostMarkdown postId
  doc <-
    txt
      & Pandoc.readMarkdown
        Pandoc.def
          { Pandoc.readerStandalone = True,
            Pandoc.readerExtensions
          }
      & runPandocM

  void $
    doc & Pandoc.walkM \(x :: Pandoc.Inline) -> case x of
      Pandoc.Link _attr kids (target, _) -> do
        return x
      _ -> return x

  return doc
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
