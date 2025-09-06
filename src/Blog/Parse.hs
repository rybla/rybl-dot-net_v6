{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blog.Parse
  ( parsePost,
  )
where

import Blog.Pandoc (fromDocError)
import qualified Blog.Pandoc as Pandoc
import Blog.Tree
import Blog.Utility (makeValidIdent, parseUriM)
import Control.Lens (to, (.~), (^.), _1)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify, runStateT)
import Control.Monad.Writer (MonadIO, execWriterT, tell)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Network
import qualified Network.HTTP.Client.TLS as Network
import qualified Network.URI as URI
import Service.Favicon (FaviconService)
import qualified Service.Favicon as Favicon
import Text.Pandoc (Pandoc (..), PandocMonad)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import Text.Pandoc.Walk

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
          >=> addTableOfContents
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

addReferencesSection :: (PandocMonad m) => Pandoc -> m Pandoc
addReferencesSection doc = do
  refs :: [(Pandoc.Inline, Text)] <-
    doc
      & ( walkM \(x :: Pandoc.Inline) -> case x of
            Pandoc.Link _attr _kids (urlText, _target) -> do
              _url <- urlText & Text.unpack & parseUriM & fromDocError
              tell [(x, urlText)]
              return x
            Pandoc.Image _attr kids target@(urlText, _target) -> do
              _url <- urlText & Text.unpack & parseUriM & fromDocError
              tell [(Pandoc.Link mempty kids target, urlText)]
              return x
            _ -> return x
        )
      & execWriterT
  case doc of
    Pandoc meta blocks -> do
      return . Pandoc meta $
        blocks
          ++ [ Pandoc.Header 1 mempty [Pandoc.Str "References"],
               Pandoc.BulletList $ refs <&> \(x, _url) -> [Pandoc.Plain [x]]
             ]

addLinkFavicons :: forall s m. (FaviconService s, PandocMonad m, MonadIO m) => Network.Manager -> Pandoc -> m Pandoc
addLinkFavicons manager = walkM \(x :: Pandoc.Inline) -> case x of
  Pandoc.Link attr kids target@(urlText, _target) -> do
    url <- urlText & Text.unpack & parseUriM & fromDocError
    faviconInfo <-
      manager & Favicon.cache @s url & fromDocError >>= \case
        Just faviconInfo -> return faviconInfo
        Nothing -> return Favicon.missingFaviconInfo

    putStrLn ("favicon url = " ++ (faviconInfo ^. Favicon.internalIconUri . to (URI.escapeURIString URI.isUnescapedInURI . show))) & liftIO

    let iconKid =
          Pandoc.Image
            ("", ["favicon"], [])
            [Pandoc.Str $ faviconInfo ^. Favicon.iconUri . to (Text.pack . show)]
            (faviconInfo ^. Favicon.internalIconUri . to (Text.pack . URI.escapeURIString URI.isUnescapedInURI . show), "")
    return $ Pandoc.Link attr ([iconKid] ++ kids) target
  _ -> return x

type TocNode = (Int, Text, [Pandoc.Inline])

addTableOfContents :: (PandocMonad m, MonadIO m) => Pandoc -> m Pandoc
addTableOfContents doc = do
  title <- doc & Pandoc.getMetaValue "title" <&> Pandoc.stringify
  (doc, Tree _ tocKids) <-
    doc
      & walkM
        ( \(x :: Pandoc.Block) -> case x of
            Pandoc.Header level attr kids -> do
              let ident = Pandoc.stringify kids & Pandoc.textToIdentifier mempty
              modify (addHeaderToToc (level, ident, kids))
              return $ Pandoc.Header level (attr & _1 .~ ident) kids
            _ -> return x
        )
      & (`runStateT` Cursor [] (Tree (0 :: Int, "title", [Pandoc.Str title]) []))
      <&> (<&> unCursor)

  return $
    Pandoc
      (Pandoc.pandocMeta doc)
      ( concat
          [ [Pandoc.OrderedList orderedListStyle (renderToc <$> tocKids)],
            Pandoc.pandocBlocks doc
          ]
      )
  where
    addHeaderToToc :: TocNode -> Cursor TocNode -> Cursor TocNode
    addHeaderToToc x c
      | (x ^. _1) > (c ^. cursorTree . treeVal . _1) =
          -- new node is new last child of focus
          Cursor (Tooth (c ^. cursorTree . treeVal) (c ^. cursorTree . treeKids) [] : c ^. cursorPath) (Tree x [])
      | otherwise =
          -- new node is sibling of parent of focus
          addHeaderToToc x (moveUpCursor c)

    renderToc :: Tree TocNode -> [Pandoc.Block]
    renderToc (Tree (_, ident, xs) []) =
      [ Pandoc.Plain [Pandoc.Link mempty xs (Text.pack $ "#" ++ Text.unpack ident, "")]
      ]
    renderToc (Tree (_, ident, xs) kids) =
      [ Pandoc.Plain [Pandoc.Link mempty xs (Text.pack $ "#" ++ Text.unpack ident, "")],
        Pandoc.OrderedList orderedListStyle (renderToc <$> kids)
      ]

    orderedListStyle = (1, Pandoc.Decimal, Pandoc.Period)
