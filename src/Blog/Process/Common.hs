{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Process.Common where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
import Blog.Tree
import Blog.Utility
import Control.Lens hiding (preview)
import Control.Monad.Except (MonadError)
import Control.Monad.State (modify, runStateT)
import Control.Monad.Writer (MonadIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as Network
import qualified Network.URI as URI
import Service.Favicon (FaviconService)
import qualified Service.Favicon as Favicon
import Service.Preview (PreviewService)
import qualified Service.Preview as Preview
import Text.Pandoc (Pandoc (..))
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, (<+>))

addReferencesSection ::
  (MonadError Doc m) =>
  [Link] -> Pandoc -> m Pandoc
addReferencesSection outLinks doc | null outLinks = pure doc
addReferencesSection outLinks (Pandoc meta blocks) = do
  return . Pandoc meta $
    blocks
      ++ [ Pandoc.Header 1 mempty [Pandoc.Str "References"],
           Pandoc.BulletList $
             outLinks & reverse <&> \link ->
               [ Pandoc.Plain
                   [ Pandoc.Link
                       (mempty & Pandoc.attrData %~ ([("inReference", ""), ("noLinkPreview", "")] ++))
                       (link & linkLabel)
                       (link & linkUri & showText, "")
                   ]
               ]
         ]

addCitationsSection ::
  (MonadError Doc m) =>
  [Link] -> Pandoc -> m Pandoc
addCitationsSection inLinks doc | null inLinks = pure doc
addCitationsSection inLinks (Pandoc meta blocks) = do
  return $
    Pandoc meta $
      concat $
        [ blocks,
          [Pandoc.Header 1 mempty [Pandoc.Str "Citations"]],
          if inLinks & null
            then []
            else
              [ Pandoc.BulletList $
                  inLinks & reverse <&> \link ->
                    [ Pandoc.Plain
                        [ Pandoc.Link
                            (mempty & Pandoc.attrData %~ ([("inCitation", ""), ("noLinkPreview", "")] ++))
                            (link & linkLabel)
                            (link & linkUri & showText, "")
                        ]
                    ]
              ]
        ]

addLinkFavicons ::
  forall m.
  (FaviconService, MonadError Doc m, MonadIO m) =>
  Network.Manager -> Pandoc -> m Pandoc
addLinkFavicons manager = Pandoc.walkM \(x :: Pandoc.Inline) -> case x of
  Pandoc.Link attr kids target@(urlText, _) | attr ^. Pandoc.attrData . to (not . ("noLinkFavicon" `elem`) . (fst <$>)) -> do
    url <- urlText & Text.unpack & parseUriReferenceM
    faviconInfo <- manager & Favicon.cache url
    logM "addLinkFavicons" $ showDoc url <+> "~~>" <+> showDoc faviconInfo
    let iconKid =
          Pandoc.Image
            ("", ["favicon"], [])
            [Pandoc.Str $ faviconInfo & Favicon.mirrorIconRef & unUriReference & showText]
            (faviconInfo & Favicon.mirrorIconRef & unUriReference & show & URI.escapeURIString URI.isUnescapedInURI & Text.pack, "")
    return $ Pandoc.Link attr ([iconKid] ++ kids) target
  _ -> return x

addLinkPreviews ::
  forall m.
  (PreviewService, MonadError Doc m, MonadIO m) =>
  Network.Manager -> Pandoc -> m Pandoc
addLinkPreviews manager = Pandoc.walkM \(x :: Pandoc.Inline) -> case x of
  Pandoc.Link attr _kids _target@(urlText, _) | attr ^. Pandoc.attrData . to (not . ("noLinkPreview" `elem`) . (fst <$>)) -> do
    url <- urlText & Text.unpack & parseUriReferenceM
    preview <- Preview.cache url manager
    return $
      Pandoc.Span
        mempty
        [ x,
          Pandoc.Span
            (mempty & Pandoc.attrClasses %~ (["sidenote", "preview"] ++))
            [ Pandoc.Span
                (mempty & Pandoc.attrClasses %~ (["preview-title"] ++))
                [Pandoc.Emph [Pandoc.Link mempty [Pandoc.Str (preview.title & Text.pack)] (urlText, "_blank")]],
              Pandoc.Span
                (mempty & Pandoc.attrClasses %~ (["preview-description"] ++))
                [Pandoc.Str (preview.description & Text.pack)]
            ]
        ]
  _ -> return x

type TocNode = (Int, Text, [Pandoc.Inline])

addTableOfContents ::
  (MonadError Doc m) =>
  Pandoc -> m Pandoc
addTableOfContents doc0 = do
  title <- doc0 & Pandoc.getMetaValue "title" <&> Pandoc.stringify
  (doc1, Tree _ tocKids) <-
    doc0
      & Pandoc.walkM
        ( \(x :: Pandoc.Block) -> case x of
            Pandoc.Header level attr kids -> do
              let ident = Pandoc.stringify kids & Pandoc.textToIdentifier mempty
              modify (addHeaderToToc (level, ident, kids))
              return $ Pandoc.Header level (attr & Pandoc.attrId .~ ident) kids
            _ -> return x
        )
      & (`runStateT` Cursor [] (Tree (0 :: Int, "title", [Pandoc.Str title]) []))
      <&> (<&> unCursor)

  return $
    Pandoc
      (Pandoc.pandocMeta doc1)
      ( concat
          [ [ Pandoc.Div
                (mempty & Pandoc.attrClasses %~ ("table-of-contents" :))
                [ Pandoc.Para [Pandoc.Underline [Pandoc.Str "Table of Contents"]],
                  Pandoc.OrderedList orderedListStyle (renderToc <$> tocKids)
                ]
            ],
            Pandoc.pandocBlocks doc1
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
      [ Pandoc.Plain [Pandoc.Link (mempty & Pandoc.attrData %~ ([("noLinkFavicon", ""), ("noLinkPreview", "")] ++)) xs (Text.pack $ "#" ++ Text.unpack ident, "")]
      ]
    renderToc (Tree (_, ident, xs) kids) =
      [ Pandoc.Plain [Pandoc.Link (mempty & Pandoc.attrData %~ ([("noLinkFavicon", ""), ("noLinkPreview", "")] ++)) xs (Text.pack $ "#" ++ Text.unpack ident, "")],
        Pandoc.OrderedList orderedListStyle (renderToc <$> kids)
      ]

    orderedListStyle = (1, Pandoc.Decimal, Pandoc.Period)

renderAbstract :: [Pandoc.Block] -> [Pandoc.Block]
renderAbstract blocks =
  concat
    [ [ Pandoc.Para
          [Pandoc.Underline [Pandoc.Str "Abstract"]]
      ],
      blocks
    ]
    <&> Pandoc.walk \(x :: Pandoc.Inline) -> case x of
      Pandoc.Link attr kids target -> Pandoc.Link (attr & Pandoc.attrData %~ ([("noLinkFavicon", "")] ++)) kids target
      _ -> x

makePubDate :: Time.Day -> Pandoc.Block
makePubDate pubDate =
  Pandoc.Para
    [ Pandoc.Underline [Pandoc.Str "Published:"],
      Pandoc.Str " ",
      Pandoc.Str $ pubDate & show & Text.pack
    ]

renderPubDate :: [Text] -> Pandoc.Block
renderPubDate tags =
  Pandoc.Para
    [ Pandoc.Underline [Pandoc.Str "Tags:"],
      Pandoc.Str " ",
      Pandoc.Str $ tags & Text.intercalate ", "
    ]

removeCommentBlocks :: Pandoc -> Pandoc
removeCommentBlocks = Pandoc.walk \(x :: [Pandoc.Block]) ->
  x & filter \case
    Pandoc.Div attr _ -> not $ attr ^. Pandoc.attrClasses . to ("comment" `elem`)
    _ -> True

renderPostHeader :: Post -> [Pandoc.Block]
renderPostHeader post =
  concat
    [ [ Pandoc.Header
          1
          mempty
          [ Pandoc.Link
              (mempty & Pandoc.attrData %~ ([("noLinkFavicon", ""), ("noLinkPreview", "")] ++))
              [Pandoc.Str post._postTitle]
              (showText post._postHref, mempty)
          ]
      ],
      [ Pandoc.Div
          (mempty & Pandoc.attrClasses %~ (["header-info"] ++))
          $ concat
            [ [makePubDate post._postPubDate],
              [renderPubDate post._postTags],
              [renderAbstract abstract | abstract <- post._postAbstract & refold] & concat
            ]
      ]
    ]
