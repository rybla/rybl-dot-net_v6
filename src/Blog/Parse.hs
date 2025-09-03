{-# LANGUAGE ScopedTypeVariables #-}

module Blog.Parse
  ( parsePost,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Except (throwError)
import Control.Monad.Writer (execWriterT, tell)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI (URI, parseURI)
import Text.Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import Text.Pandoc.Walk
import Text.PrettyPrint.HughesPJClass (brackets, doubleQuotes, hcat, parens, render, text, (<+>))

parsePost :: (PandocMonad m) => Text -> m Pandoc
parsePost =
  readMarkdown
    def
      { readerStandalone = True,
        readerExtensions
      }
    >=> addReferencesSection
  where
    readerExtensions =
      extensionsFromList
        [ -- metadata
          Ext_yaml_metadata_block,
          -- styles
          Ext_mark,
          Ext_strikeout,
          Ext_subscript,
          Ext_superscript,
          Ext_footnotes,
          -- groupings
          Ext_tex_math_dollars,
          Ext_backtick_code_blocks,
          Ext_bracketed_spans,
          Ext_fenced_divs,
          -- attributes
          Ext_attributes,
          Ext_fenced_code_attributes,
          Ext_header_attributes,
          Ext_inline_code_attributes,
          Ext_link_attributes,
          Ext_mmd_link_attributes,
          Ext_raw_attribute
        ]

addReferencesSection :: (PandocMonad m) => Pandoc -> m Pandoc
addReferencesSection doc = do
  refs :: [(Inline, Text)] <-
    doc
      & ( walkM \(x :: Inline) -> case x of
            Link _attr kids (urlText, _target) -> do
              let label = Pandoc.stringify kids
              -- url <- urlText & show & parseURI & maybe (throwError . PandocAppError . Text.pack . render $ "invalid URL" <+> (doubleQuotes . text . Text.unpack $ urlText) <+> "in link" <+> hcat [brackets . text . Text.unpack $ label, parens . text . Text.unpack $ urlText]) return
              tell [(x, urlText)]
              return x
            Image _attr kids target@(urlText, _target) -> do
              let label = Pandoc.stringify kids
              -- url <- urlText & show & parseURI & maybe (throwError . PandocAppError . Text.pack . render $ "invalid URL" <+> (doubleQuotes . text . Text.unpack $ urlText) <+> "in link" <+> hcat [brackets . text . Text.unpack $ label, parens . text . Text.unpack $ urlText]) return
              tell [(Link mempty kids target, urlText)]
              return x
            _ -> return x
        )
      & execWriterT
  case doc of
    Pandoc meta blocks -> do
      return . Pandoc meta $
        blocks
          <> [ Header 2 mempty [Str "References"],
               BulletList $ refs <&> \(x, _url) -> [Plain [x]]
             ]
