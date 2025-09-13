{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Common where

import qualified Blog.Paths as Paths
import Blog.Utility
import Control.Category ((>>>))
import Control.Lens hiding ((<.>))
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON, ToJSON, parseJSON)
import Data.Aeson.Types (toJSON)
import Data.Text (Text)
import Network.URI (URI)
import qualified Network.URI as URI
import System.FilePath ((<.>), (</>))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, Pretty, brackets, hcat, pPrint, text)

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

data Link = Link
  { linkLabel :: [Pandoc.Inline],
    linkUri :: URI
  }
  deriving (Show)

data Post = Post
  { postId :: PostId,
    postHref :: URI,
    postTitle :: Text,
    postPubDate :: Text,
    postTags :: [Text],
    _postDoc :: Pandoc.Pandoc
  }

makeLenses ''Post

toPostHref :: (MonadError Doc m) => PostId -> m URI
toPostHref postId = parseUriReferenceM (Paths.online.post.here </> (postId & unPostId & makeValidIdent & toHtmlFileName))

toPostDataFilePath :: PostId -> FilePath
toPostDataFilePath postId = Paths.offline.post_data.here </> (postId & unPostId & makeValidIdent & toDataFileName)

toMarkdownFileName :: FilePath -> FilePath
toMarkdownFileName = (<.> ".md")

toDataFileName :: FilePath -> FilePath
toDataFileName = (<.> ".json")

toHtmlFileName :: FilePath -> FilePath
toHtmlFileName = (<.> ".html")

toFaviconInfoFilePath :: URI -> FilePath
toFaviconInfoFilePath uri = Paths.offline.preview.here </> (uri & uriRootAndPath & makeValidIdent & toDataFileName)

toPostFilePath :: PostId -> FilePath
toPostFilePath postId = Paths.offline.post.here </> (postId & unPostId & makeValidIdent & toHtmlFileName)
