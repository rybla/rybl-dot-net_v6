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
import qualified Blog.Common as Config
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Process.Common
import Blog.Utility
import Control.Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State (MonadIO, MonadState, gets)
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Network
import Network.URI (URI)
import Service.Favicon (FaviconService)
import Service.Preview (PreviewService)
import System.FilePath ((</>))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, (<+>))

processPost ::
  (FaviconService, PreviewService, MonadError Doc m, MonadState env m, MonadIO m) =>
  Lens' env Network.Manager ->
  Lens' env (Map URI [Link]) ->
  Lens' env (Map URI [Link]) ->
  Lens' env Post ->
  m ()
processPost manager outLinks inLinks post = do
  gets (^. post) >>= \post' ->
    logM "processPost" $ "title =" <+> textDoc post'._postTitle

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

  addPostSignatureSection

  whenM (gets (^. post . postTableOfContentsEnabled)) do
    post . postDoc %=* addTableOfContents

  addPostHeader

  post . postDoc %=* addLinkFavicons mgr

  return ()
  where
    addPostHeader = do
      post' <- gets (^. post)
      post . postDoc . Pandoc._pandocBlocks %= \blocks ->
        concat
          [ renderPostHeader post',
            blocks
          ]
      pure ()

    addPostSignatureSection = do
      post' <- gets (^. post)
      post . postDoc . Pandoc._pandocBlocks %= \blocks ->
        concat
          [ blocks,
            [ Pandoc.Header 1 mempty [Pandoc.Str "Signature"],
              Pandoc.Para
                [ Pandoc.Str "The following code block is the ",
                  Pandoc.Link mempty [Pandoc.Str "Ed25519 signature"] ("https://en.wikipedia.org/wiki/EdDSA#Ed25519", "_blank"),
                  Pandoc.Str " of this post's ",
                  Pandoc.Link mempty [Pandoc.Str "markdown content"] (post'._postMarkdownHref & showText, ""),
                  Pandoc.Str " encoded in base 64, using my ",
                  Pandoc.Emph [Pandoc.Str "secret key"],
                  Pandoc.Str " and ",
                  Pandoc.Link mempty [Pandoc.Str "public key"] (showText Config.publicKeyUri, "_blank"),
                  Pandoc.Str "."
                ],
              Pandoc.CodeBlock mempty (Text.pack $ showByteArrayAsBase64 $ post'._postMarkdownSignature),
              Pandoc.Para
                [ Pandoc.Str "See ",
                  Pandoc.Link
                    mempty
                    [Pandoc.Str "Signature"]
                    (Text.pack $ Paths.onlineSite.page.here </> "signature.html", ""),
                  Pandoc.Str " for more information."
                ]
            ]
          ]
