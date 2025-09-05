{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blog.Parse
  ( parsePost,
  )
where

import Blog.Utility (markdownImage, markdownLink)
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer (MonadIO, execWriterT, tell)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Network
import qualified Network.HTTP.Client.TLS as Network
import qualified Network.URI as URI
import Service.Favicon (FaviconService (fetchFaviconInfo))
import Text.Pandoc (Pandoc (..), PandocError (..), PandocMonad)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import Text.Pandoc.Walk
import Text.PrettyPrint.HughesPJClass

parsePost :: forall s m. (FaviconService s, PandocMonad m, MonadIO m) => Text -> m Pandoc
parsePost txt = do
  manager <- Network.newManager Network.tlsManagerSettings & liftIO
  txt
    & ( Pandoc.readMarkdown
          Pandoc.def
            { Pandoc.readerStandalone = True,
              Pandoc.readerExtensions
            }
          >=> addReferencesSection
          >=> addLinkFavicons @s manager
      )
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
          Pandoc.Ext_footnotes,
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

addReferencesSection :: (PandocMonad m) => Pandoc -> m Pandoc
addReferencesSection doc = do
  refs :: [(Pandoc.Inline, Text)] <-
    doc
      & ( walkM \(x :: Pandoc.Inline) -> case x of
            Pandoc.Link _attr kids (urlText, _target) -> do
              let labelText = Pandoc.stringify kids
              _url <- urlText & show & URI.parseURI & maybe (throwPandocError $ "invalid URL" <+> (doubleQuotes . text . Text.unpack $ urlText) <+> "in link" <+> text (markdownLink (Text.unpack labelText) (Text.unpack urlText))) return
              tell [(x, urlText)]
              return x
            Pandoc.Image _attr kids target@(urlText, _target) -> do
              let labelText = Pandoc.stringify kids
              _url <- urlText & show & URI.parseURI & maybe (throwPandocError $ "invalid URL" <+> (doubleQuotes . text . Text.unpack $ urlText) <+> "in image" <+> text (markdownImage (Text.unpack labelText) (Text.unpack urlText))) return
              tell [(Pandoc.Link mempty kids target, urlText)]
              return x
            _ -> return x
        )
      & execWriterT
  case doc of
    Pandoc meta blocks -> do
      return . Pandoc meta $
        blocks
          ++ [ Pandoc.Header 2 mempty [Pandoc.Str "References"],
               Pandoc.BulletList $ refs <&> \(x, _url) -> [Pandoc.Plain [x]]
             ]

addLinkFavicons :: forall s m. (FaviconService s, PandocMonad m, MonadIO m) => Network.Manager -> Pandoc -> m Pandoc
addLinkFavicons manager = walkM \(x :: Pandoc.Inline) -> case x of
  Pandoc.Link attr kids target@(urlText, _target) -> do
    let labelText = Pandoc.stringify kids
    url <- urlText & show & URI.parseURI & maybe (throwPandocError $ "invalid URL" <+> (doubleQuotes . text . Text.unpack $ urlText) <+> "in link" <+> text (markdownLink (Text.unpack labelText) (Text.unpack urlText))) return
    
    faviconInfo <-
      fetchFaviconInfo (Proxy @s) url manager
        & runExceptT
        >>= \case
          Left err -> throwPandocError $ "Failed to fetch favicon info:" <+> err
          Right Nothing -> return undefined
          Right (Just faviconInfo) -> return faviconInfo
    let iconKid = undefined
    return $ Pandoc.Link attr ([iconKid] ++ kids) target
  _ -> return x

throwPandocError :: (PandocMonad m) => Doc -> m a
throwPandocError = throwError . PandocAppError . Text.pack . render
