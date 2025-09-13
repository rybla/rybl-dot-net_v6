{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Parse.Post where

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
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI (URI)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, pPrint, (<+>))

parse ::
  (MonadIO m, MonadError Doc m, MonadState env m) =>
  Lens' env (Map URI [Link]) ->
  Lens' env (Map URI [Link]) ->
  PostId ->
  Text ->
  m Post
parse outLinks inLinks postId postText = do
  logM "Post.parse" $ "postId =" <+> pPrint postId
  doc <-
    postText
      & Pandoc.readMarkdown
        Pandoc.def
          { Pandoc.readerStandalone = True,
            Pandoc.readerExtensions
          }
      & runPandocM

  postHref <- toPostHref postId
  postTitle <- doc & Pandoc.getMetaValueSuchThat Pandoc.fromMetaString "title"
  postPubDate <- doc & Pandoc.getMetaValueSuchThat Pandoc.fromMetaString "pubDate"
  postTags <- doc & Pandoc.getMetaValueSuchThat Pandoc.fromMetaListString "tags"
  postAbstract <- doc & Pandoc.getMetaValueMaybeSuchThat Pandoc.fromMetaString "abstract"

  void $
    doc & Pandoc.walkM \(x :: Pandoc.Inline) -> case x of
      Pandoc.Link _attr kids (refText, _) -> do
        ref <- refText & Text.unpack & parseUriReferenceM
        let outLink = Link kids ref
        outLinks . at postHref %= maybe (Just [outLink]) (Just . (outLink :))
        let inLink = Link kids postHref
        inLinks . at ref %= maybe (Just [inLink]) (Just . (inLink :))
        return x
      Pandoc.Image _attr kids (refText, _) -> do
        ref <- refText & Text.unpack & parseUriReferenceM
        let outLink = Link kids ref
        outLinks . at postHref %= maybe (Just [outLink]) (Just . (outLink :))
        let inLink = Link kids postHref
        inLinks . at ref %= maybe (Just [inLink]) (Just . (inLink :))
        return x
      _ -> return x

  return
    Post
      { postId,
        postHref,
        postTitle,
        postPubDate,
        postAbstract,
        postTags,
        _postDoc = doc
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
