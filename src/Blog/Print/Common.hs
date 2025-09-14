{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Print.Common where

import qualified Blog.Paths as Paths
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import Text.DocTemplates (TemplateMonad (..))

newtype BlogTemplateMonad m a = BlogTemplateMonad {unBlogTemplateMonad :: m a}

deriving newtype instance (Functor m) => Functor (BlogTemplateMonad m)

deriving newtype instance (Applicative m) => Applicative (BlogTemplateMonad m)

deriving newtype instance (Monad m) => Monad (BlogTemplateMonad m)

deriving newtype instance (MonadIO m) => MonadIO (BlogTemplateMonad m)

instance (MonadIO m) => TemplateMonad (BlogTemplateMonad m) where
  getPartial fp = TextIO.readFile (Paths.offlineSite.template.here </> fp) & liftIO
