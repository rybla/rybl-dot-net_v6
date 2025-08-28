{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Paths where

import Blog.PathsTh (MakeRootParams (..), makeRoot)
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
baseUri = "https://rybl.net" & Maybe.fromJust . URI.parseURI

baseFaviconUri :: URI
baseFaviconUri =
  ("favicon.ico" & Maybe.fromJust . URI.parseRelativeReference)
    `URI.relativeTo` baseUri

makeRoot
  [ MakeRootParams
      { valName = "offline",
        nodeType = [t|String|],
        toNode = [|("work" </>)|]
      },
    MakeRootParams
      { valName = "online",
        nodeType = [t|URI|],
        toNode =
          [|
            \s ->
              (s & Maybe.fromJust . URI.parseRelativeReference)
                `URI.relativeTo` baseUri
            |]
      }
  ]
  [ Node "favicon" [],
    Node "input" $
      [ Node "post" []
      ]
  ]

listPostFilePaths :: IO [FilePath]
listPostFilePaths =
  listDirectory offline.input.post.here
    <&> (((offline.input.post.here </>) <$>) . filter (".md" `isSuffixOf`))
