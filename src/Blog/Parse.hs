module Blog.Parse
  ( parsePost,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State.Strict (evalState, evalStateT)
import Data.Function ((&))
import Data.Text (Text)
import Text.Pandoc

parsePost :: (MonadError PandocError m) => Text -> m Pandoc
parsePost txt =
  readMarkdown
    def
      { readerStandalone = True,
        readerExtensions
      }
    txt
    & unPandocPure
    & runExceptT
    & (`evalStateT` def)
    & (`evalState` def)
    & either throwError return
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
