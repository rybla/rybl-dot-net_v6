{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
import Data.Time (Day)
import Network.URI (URI)
import qualified Network.URI as URI
import System.FilePath ((<.>), (</>))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, Pretty, brackets, hcat, pPrint, text)

newtype PostId = PostId {unPostId :: String}
  deriving (Show, Eq, Ord)

instance Pretty PostId where
  pPrint (PostId pid) = hcat ["PostId", brackets (text pid)]

newtype PageId = PageId {unPageId :: String}
  deriving (Show, Eq, Ord)

instance Pretty PageId where
  pPrint (PageId pid) = hcat ["PageId", brackets (text pid)]

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
  { _postId :: PostId,
    _postHref :: URI,
    _postTitle :: Text,
    _postPubDate :: Day,
    _postTags :: [Text],
    _postAbstract :: Maybe [Pandoc.Block],
    _postDoc :: Pandoc.Pandoc
  }

makeLenses ''Post

data Page = Page
  { _pageId :: PageId,
    _pageHref :: URI,
    _pageTitle :: Text,
    _pageDoc :: Pandoc.Pandoc
  }

makeLenses ''Page

-- Post

toPostHref :: (MonadError Doc m) => PostId -> m URI
toPostHref postId = parseUriReferenceM (Paths.online.post.here </> (postId & unPostId & makeValidIdent & toHtmlFileName))

toPostMarkdownFilePath :: PostId -> FilePath
toPostMarkdownFilePath postId = Paths.offline.post_markdown.here </> (postId & unPostId & makeValidIdent & toMarkdownFileName)

toPostFilePath :: PostId -> FilePath
toPostFilePath postId = Paths.offline.post.here </> (postId & unPostId & makeValidIdent & toHtmlFileName)

-- Page

toPageHref :: (MonadError Doc m) => PageId -> m URI
toPageHref postId = parseUriReferenceM (Paths.online.page.here </> (postId & unPageId & makeValidIdent & toHtmlFileName))

toPageMarkdownFilePath :: PageId -> FilePath
toPageMarkdownFilePath postId = Paths.offline.page_markdown.here </> (postId & unPageId & makeValidIdent & toMarkdownFileName)

toPageFilePath :: PageId -> FilePath
toPageFilePath postId = Paths.offline.page.here </> (postId & unPageId & makeValidIdent & toHtmlFileName)

-- Favicon

toFaviconInfoFilePath :: URI -> FilePath
toFaviconInfoFilePath uri = Paths.offline.preview.here </> (uri & uriRootAndPath & makeValidIdent & toDataFileName)

-- generic

toMarkdownFileName :: FilePath -> FilePath
toMarkdownFileName = (<.> ".md")

toDataFileName :: FilePath -> FilePath
toDataFileName = (<.> ".json")

toHtmlFileName :: FilePath -> FilePath
toHtmlFileName = (<.> ".html")
