{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Common where

import Control.Category ((>>>))
import Control.Lens
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON, parseJSON)
import Data.Aeson.Types (toJSON)
import Network.URI (URI)
import qualified Network.URI as URI
import Text.PrettyPrint.HughesPJClass (Pretty, brackets, hcat, pPrint, text)

newtype PostId = PostId {unPostId :: String}
  deriving (Show, Eq, Ord)

instance Pretty PostId where
  pPrint (PostId pid) = hcat ["Post", brackets (text pid)]

newtype UriReference = UriReference {unUriReference :: URI}
  deriving (Show)

instance ToJSON UriReference where
  toJSON ur = ur & unUriReference & toJSON

instance FromJSON UriReference where
  parseJSON = parseJSON @String >=> URI.parseURIReference >>> maybe mempty (pure . UriReference)
