{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Parse.Common where

import Control.Lens
import Network.URI (URI)
import qualified Text.Pandoc as Pandoc

data Link = Link
  { _linkLabel :: [Pandoc.Inline],
    _linkUri :: URI
  }
  deriving (Show)

makeLenses ''Link
