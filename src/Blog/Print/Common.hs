{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Print.Common where

import qualified Blog.Paths as Paths
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import Text.DocTemplates (TemplateMonad (..))
import qualified Text.DocTemplates as DocTemplates
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Highlighting as Highlighting

newtype BlogTemplateMonad m a = BlogTemplateMonad {unBlogTemplateMonad :: m a}

deriving newtype instance (Functor m) => Functor (BlogTemplateMonad m)

deriving newtype instance (Applicative m) => Applicative (BlogTemplateMonad m)

deriving newtype instance (Monad m) => Monad (BlogTemplateMonad m)

deriving newtype instance (MonadIO m) => MonadIO (BlogTemplateMonad m)

instance (MonadIO m) => TemplateMonad (BlogTemplateMonad m) where
  getPartial fp = TextIO.readFile (Paths.offlineSite.template.here </> fp) & liftIO

commonWriterOptions :: Maybe (Pandoc.Template Text) -> DocTemplates.Context Text -> Pandoc.WriterOptions
commonWriterOptions mb_template vars =
  def
    { Pandoc.writerTemplate = mb_template,
      Pandoc.writerVariables = vars,
      Pandoc.writerHTMLMathMethod = Pandoc.MathJax Pandoc.defaultMathJaxURL,
      Pandoc.writerHighlightStyle = Just Highlighting.espresso
    }
