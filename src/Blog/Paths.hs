{-# LANGUAGE OverloadedStrings #-}

module Blog.Paths
  ( inputDirPath,
    postDirPath,
    listPostFilePaths,
    baseUri,
    faviconUri,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import Network.URI (URI, parseURI, relativeTo, parseRelativeReference)
import System.Directory (listDirectory)
import System.FilePath ((</>))

inputDirPath :: FilePath
inputDirPath = "input"

postDirPath :: FilePath
postDirPath = inputDirPath </> "post"

listPostFilePaths :: IO [FilePath]
listPostFilePaths =
  listDirectory postDirPath
    <&> (((postDirPath </>) <$>) . filter (".md" `isSuffixOf`))

baseUri :: URI
baseUri = "https://rybl.net" & parseURI & fromJust

faviconUri :: URI
faviconUri = ("/favicon" & parseRelativeReference & fromJust) `relativeTo` baseUri
