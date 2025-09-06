{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Paths where

import qualified Blog.Config as Config
import Blog.PathsTh (MakeRootParams (..), makeRoot)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import qualified Data.Maybe as Maybe
import Data.Tree (Tree (..))
import Network.URI (URI)
import qualified Network.URI as URI
import System.Directory (listDirectory)
import System.FilePath ((</>))

baseUri :: URI
baseUri = case Config.mode of
  Config.Production -> "https://rybl.net" & Maybe.fromJust . URI.parseURI
  Config.Development -> "http://localhost:8080" & Maybe.fromJust . URI.parseURI

baseFaviconUri :: URI
baseFaviconUri = relUri `URI.relativeTo` baseUri
  where
    relUri = "favicon.ico" & URI.parseRelativeReference & Maybe.fromJust

baseFaviconFormat :: String
baseFaviconFormat = "ico"

missingFaviconUri :: URI
missingFaviconUri = relUri `URI.relativeTo` baseUri
  where
    relUri = "missing.ico" & URI.parseRelativeReference & Maybe.fromJust

missingFaviconFormat :: String
missingFaviconFormat = "ico"

makeRoot
  [ MakeRootParams
      { valName = "offline",
        nodeType = [t|String|],
        toNode = [|("asset" </>)|]
      },
    MakeRootParams
      { valName = "online",
        nodeType = [t|URI|],
        toNode =
          [|
            \s ->
              let relUri = s & URI.parseRelativeReference & Maybe.fromJust
               in relUri `URI.relativeTo` baseUri
            |]
      }
  ]
  [ Node "post_markdown" [],
    Node "post" [],
    Node "favicon" [],
    Node "template" []
  ]

listPostFilePaths :: (MonadIO m) => m [FilePath]
listPostFilePaths =
  listDirectory offline.post_markdown.here
    & liftIO
    <&> (((offline.post_markdown.here </>) <$>) . filter (".md" `isSuffixOf`))
