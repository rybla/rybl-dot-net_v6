{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Parse.Post where

import Blog.Common
import Blog.Pandoc (runPandocM)
import Blog.Parse.Common
import qualified Blog.Paths as Paths
import Blog.Utility (logM, parseUriReferenceM)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadIO)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Network.URI (URI)
import System.FilePath ((</>))
import Text.Pandoc (Pandoc (..))
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, pPrint, (<+>))

data Env = Env
  { _outLinks :: Map URI [Link],
    _inLinks :: Map URI [Link]
  }
  deriving (Show)

newEnv :: Env
newEnv = do
  Env
    { _outLinks = Map.empty,
      _inLinks = Map.empty
    }

makeLenses ''Env

parsePost ::
  forall m.
  (MonadIO m, MonadError Doc m, MonadState Env m) =>
  PostId -> m Pandoc
parsePost postId = do
  logM "parsePost" $ "postId =" <+> pPrint postId
  postUri <- parseUriReferenceM (Paths.online.post.here </> (postId ^. unPostId . to Paths.toHtmlFileName))
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
      Pandoc.Link _attr kids (refText, _) -> do
        ref <- refText & Text.unpack & parseUriReferenceM
        let outLink = Link kids ref
        outLinks . at postUri %= maybe (Just [outLink]) (Just . (outLink :))
        let inLink = Link kids postUri
        inLinks . at ref %= maybe (Just [inLink]) (Just . (inLink :))
        return x
      Pandoc.Image _attr kids (refText, _) -> do
        ref <- refText & Text.unpack & parseUriReferenceM
        let outLink = Link kids ref
        outLinks . at postUri %= maybe (Just [outLink]) (Just . (outLink :))
        let inLink = Link kids postUri
        inLinks . at ref %= maybe (Just [inLink]) (Just . (inLink :))
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
