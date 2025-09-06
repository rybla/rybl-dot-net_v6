{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Parse.Post where

import Blog.Pandoc (runPandocM)
import Blog.Parse.Common (addLinkFavicons, addReferencesSection, addTableOfContents)
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState, gets)
import Control.Monad.Writer (MonadIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Network.HTTP.Client as Network
import qualified Network.HTTP.Client.TLS as Network
import Network.URI (URI)
import Service.Favicon (FaviconService)
import Text.Pandoc (Pandoc (..))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc)

data Link
  = InternalLink String
  | ExternalLink URI

data ParsePostEnv = ParsePostEnv
  { manager :: Network.Manager,
    _outLinks :: Map String [Link],
    _inLinks :: Map String [Link]
  }

newParsePostEnv :: Network.Manager -> ParsePostEnv
newParsePostEnv manager = do
  ParsePostEnv
    { manager,
      _outLinks = Map.empty,
      _inLinks = Map.empty
    }

newParsePostEnvM :: (MonadIO m) => m ParsePostEnv
newParsePostEnvM = do
  manager <- Network.newManager Network.tlsManagerSettings & liftIO
  return $ newParsePostEnv manager

makeLenses ''ParsePostEnv

parsePost ::
  forall s m.
  (FaviconService s, MonadIO m, MonadError Doc m, MonadState ParsePostEnv m) =>
  Text -> m Pandoc
parsePost txt = do
  manager <- gets manager
  doc0 <-
    txt
      & Pandoc.readMarkdown
        Pandoc.def
          { Pandoc.readerStandalone = True,
            Pandoc.readerExtensions
          }
      & runPandocM
  doc1 <- doc0 & addReferencesSection
  doc2 <- doc1 & addLinkFavicons @s manager
  doc3 <- doc2 & addTableOfContents
  return doc3
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
