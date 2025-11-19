{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Print.Index (printIndex) where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Print.Common
import Blog.Utility
import Control.Lens hiding (index)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Default (def)
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printIndex :: (MonadIO m, MonadError Doc m, MonadState env m) => Page -> m ()
printIndex page = evalIsoStateT (pairIso def) do
  templateText <- TextIO.readFile (Paths.offlineSite.template.here </> ("index" & toHtmlFileName)) & liftIO

  pageTemplate <-
    Pandoc.compileTemplate mempty templateText
      & unBlogTemplateMonad
      >>= fromEither (("compileTemplate:" <+>) . text)

  pageHtml <- do
    vars <-
      Aeson.parseEither
        Aeson.parseJSON
        ( Aeson.object
            [ ("title", page._pageTitle & Aeson.toJSON)
            ]
        )
        & fromEither (("Error when parsing template variables JSON:" <+>) . text)
    Pandoc.writeHtml5String
      (commonWriterOptions (Just pageTemplate) vars)
      (page ^. pageDoc)
      & Pandoc.lensPandocM _1

  TextIO.writeFile (Paths.offlineSite.here </> ("index" & toHtmlFileName)) pageHtml & liftIO
