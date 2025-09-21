{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Monad (filterM, (>=>))
import Control.Monad.Except (MonadError)
import Control.Monad.State (modify, runStateT)
import Control.Monad.Writer (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
import Text.PrettyPrint.HughesPJClass (Doc, doubleQuotes, text, (<+>))

commonTransformations :: (MonadIO m, MonadError Doc m) => Pandoc -> m Pandoc
commonTransformations =
  removeCommentBlocks
    >=> processCustomBlocks
    >=> processCodeBlocks

processCustomBlocks :: (MonadIO m, MonadError Doc m) => Pandoc -> m Pandoc
processCustomBlocks = Pandoc.walkM \(x :: Pandoc.Block) -> case x of
  Pandoc.Div attr blocks -> do
    logM "processCustomBlocks" $ "class =" <+> text (show (attr ^. Pandoc.attrClasses))
    let extractClass c =
          let cs = attr ^. Pandoc.attrClasses
              cs' = cs & filter (c /=)
           in if (cs' & length) < (cs & length)
                then
                  pure (c, cs')
                else
                  mempty

        lookupDataRequired c key =
          attr ^. Pandoc.attrData
            & assocList key
            & fromMaybe ("custom block" <+> doubleQuotes (textDoc c) <+> "requires data key:" <+> doubleQuotes (textDoc key))
        lookupDataOptional key =
          attr ^. Pandoc.attrData
            & assocList key
    if
      | Just (c, _cs) <- extractClass "include-code-block" -> do
          logM "processCustomBlocks: include-code-block" $ "attr =" <+> text (show attr)
          srcFilePathText <- lookupDataRequired c "src"
          let srcFilePath = Text.unpack srcFilePathText
          logM "processCustomBlocks: include-code-block" $ "src =" <+> textDoc srcFilePathText
          let extMb = filepathExtension srcFilePath <&> Text.pack
          let titleMb = lookupDataOptional "title"
          hrefMb <- lookupDataOptional "href" <&> Text.unpack & traverse parseUriReferenceM
          txt <- Text.readFile srcFilePath & liftIO
          logM "processCustomBlocks: include-code-block" $ "txt =" <+> textDoc txt
          pure . Pandoc.BlockQuote . concat $
            [ [ Pandoc.Para
                  [ let makeTitle title = Pandoc.Strong [Pandoc.Str title]
                        makeTarget href = (href & showText, "_blank")
                     in case (titleMb, hrefMb) of
                          (Just title, Just href) -> Pandoc.Link mempty [makeTitle title] (makeTarget href)
                          (Just title, Nothing) -> makeTitle title
                          (Nothing, Just href) -> Pandoc.Link mempty [makeTitle $ href & showText] (href & showText, "_blank")
                          (Nothing, Nothing) -> makeTitle $ srcFilePath & Text.pack
                  ]
              ],
              [ Pandoc.CodeBlock
                  (mempty & Pandoc.attrClasses <>~ (extMb & refold))
                  txt
              ],
              blocks
            ]
      | otherwise -> pure x
  _ -> pure x

removeCommentBlocks :: (Monad m) => Pandoc -> m Pandoc
removeCommentBlocks = Pandoc.walkM \(x :: [Pandoc.Block]) ->
  x & filterM \case
    Pandoc.Div attr _ -> return $ not $ attr ^. Pandoc.attrClasses & elem "comment"
    _ -> return True

processCodeBlocks :: (Monad m) => Pandoc -> m Pandoc
processCodeBlocks = Pandoc.walkM \(x :: Pandoc.Block) -> case x of
  Pandoc.CodeBlock attr txt ->
    pure $
      Pandoc.Div mempty $
        [ Pandoc.CodeBlock
            (attr & Pandoc.attrClasses %~ \cs -> (if null cs then ["txt"] else cs))
            txt
        ]
  _ -> pure x

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
                       (mempty & Pandoc.attrClasses <>~ ["inReference", "noLinkPreview"])
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
                            (mempty & Pandoc.attrClasses <>~ ["inCitation", "noLinkPreview"])
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
  Pandoc.Link attr kids target@(urlText, _) | not $ attr ^. Pandoc.attrClasses & (elem "noLinkFavicon") -> do
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
  Pandoc.Link attr _kids _target@(urlText, _) | not $ attr ^. Pandoc.attrClasses & (elem "noLinkPreview") -> do
    url <- urlText & Text.unpack & parseUriReferenceM
    preview <- Preview.cache url manager
    return $
      Pandoc.Span
        mempty
        [ x,
          Pandoc.Span
            (mempty & Pandoc.attrClasses %~ (["sidenote", "preview"] ++))
            [ Pandoc.Emph [Pandoc.Link mempty [Pandoc.Str (preview.title & Text.pack)] (urlText, "_blank")],
              Pandoc.RawInline (Pandoc.Format "html") "<br/>",
              Pandoc.Str (preview.description & Text.pack)
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
    Pandoc (Pandoc.pandocMeta doc1) . concat $
      [ [ Pandoc.Div mempty $
            [ Pandoc.Div
                (mempty & Pandoc.attrClasses %~ (["sidenote", "persistent", "table-of-contents"] ++))
                [ Pandoc.Para [Pandoc.Underline [Pandoc.Str "Table of Contents"]],
                  Pandoc.OrderedList orderedListStyle (renderToc <$> tocKids)
                ]
            ]
        ],
        Pandoc.pandocBlocks doc1
      ]
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
      [ Pandoc.Plain
          [ Pandoc.Link
              (mempty & Pandoc.attrClasses <>~ ["noLinkFavicon", "noLinkPreview"])
              xs
              (Text.pack $ "#" ++ Text.unpack ident, "")
          ]
      ]
    renderToc (Tree (_, ident, xs) kids) =
      [ Pandoc.Plain
          [ Pandoc.Link
              (mempty & Pandoc.attrClasses <>~ ["noLinkFavicon", "noLinkPreview"])
              xs
              (Text.pack $ "#" ++ Text.unpack ident, "")
          ],
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
      Pandoc.Link attr kids target -> Pandoc.Link (attr & Pandoc.attrClasses <>~ ["noLinkFavicon"]) kids target
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

renderPostHeader :: Post -> [Pandoc.Block]
renderPostHeader post =
  concat
    [ [ Pandoc.Header
          1
          mempty
          [ Pandoc.Link
              (mempty & Pandoc.attrClasses <>~ ["noLinkFavicon", "noLinkPreview"])
              [Pandoc.Str post._postTitle]
              (showText post._postHref, mempty)
          ]
      ],
      [ Pandoc.Div mempty $
          [ Pandoc.Div (mempty & Pandoc.attrClasses <>~ ["sidenote", "persistent", "header-info"]) . concat $
              [ [makePubDate post._postPubDate],
                [renderPubDate post._postTags],
                [renderAbstract abstract | abstract <- post._postAbstract & refold] & concat
              ]
          ]
      ]
    ]
