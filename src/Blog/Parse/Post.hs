{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Parse.Post where

import Blog.Common
import Blog.Pandoc (runPandocM)
import qualified Blog.Pandoc as Pandoc
import Blog.Parse.Common
import qualified Blog.Secret as Secret
import Blog.Utility
import Control.Lens
import Control.Monad (void, (>=>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadIO)
import qualified Crypto.PubKey.Ed25519 as Crypto
import Data.ByteArray ()
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.URI (URI)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, pPrint, (<+>))

parsePost ::
  (MonadIO m, MonadError Doc m, MonadState env m) =>
  Lens' env (Map URI Text) ->
  Lens' env (Map URI [Link]) ->
  Lens' env (Map URI [Link]) ->
  PostId ->
  Text ->
  m Post
parsePost uriLabels outLinks inLinks postId postText = do
  logM "pasePost" $ "postId =" <+> pPrint postId
  doc <-
    postText
      & Pandoc.readMarkdown
        commonReaderOptions
      & runPandocM

  let postMarkdownSignature = Crypto.sign Secret.secretKey Secret.publicKey (Text.encodeUtf8 postText)

  postHref <- toPostHref postId
  postMarkdownHref <- toPostMarkdownHref postId
  postTitle <- doc & Pandoc.getMetaValueSuchThat Pandoc.fromMetaString "title"
  uriLabels . at postHref %= \case
    Just existingLabel -> error $ "attempted to add uriLabel for post " ++ show postTitle ++ "at href " ++ show postHref ++ " a second time; the existing label is " ++ show existingLabel
    Nothing -> Just postTitle
  postPubDate <-
    doc
      & Pandoc.getMetaValueSuchThat
        (Pandoc.fromMetaString >=> maybe (throwError "expected date") pure . parseDay . Text.unpack)
        "pubDate"
  postTags <- doc & Pandoc.getMetaValueSuchThat Pandoc.fromMetaListString "tags"
  postAbstract <- doc & Pandoc.getMetaValueMaybeSuchThat Pandoc.fromMetaBlocks "abstract"
  postTableOfContentsEnabled <-
    doc
      & Pandoc.getMetaValueMaybeSuchThat Pandoc.fromMetaBool "table_of_contents"
      <&> Maybe.fromMaybe True

  void $
    doc & Pandoc.walkM \(x :: Pandoc.Inline) -> case x of
      Pandoc.Link _attr kids (refText, _) | (refText & Text.take 1) /= "#" -> do
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
      { _postId = postId,
        _postHref = postHref,
        _postTitle = postTitle,
        _postPubDate = postPubDate,
        _postAbstract = postAbstract,
        _postTags = postTags,
        _postTableOfContentsEnabled = postTableOfContentsEnabled,
        _postMarkdownHref = postMarkdownHref,
        _postMarkdownSignature = postMarkdownSignature,
        _postDoc = doc
      }
