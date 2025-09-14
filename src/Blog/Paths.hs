{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Paths where

import Blog.PathsTh (MakeRootParams (..), makeRoot)
import Data.Tree (Tree (..))
import Network.URI (URI)
import Network.URI.Static (relativeReference)
import System.FilePath ((</>))

baseFaviconUri :: URI
baseFaviconUri = [relativeReference|/favicon.ico|]

baseFaviconFilePath :: FilePath
baseFaviconFilePath = "/favicon.ico"

baseFaviconFormat :: String
baseFaviconFormat = "ico"

missingFaviconUri :: URI
missingFaviconUri = [relativeReference|/missing.ico|]

missingFaviconFilePath :: FilePath
missingFaviconFilePath = "/missing.ico"

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
        nodeType = [t|String|],
        toNode = [|("/" </>)|]
      }
  ]
  [ Node "post_markdown" [],
    Node "post" [],
    Node "favicon" [],
    Node "template" [],
    Node "preview" [],
    Node "script" [],
    Node "page_markdown" [],
    Node "page" []
  ]
