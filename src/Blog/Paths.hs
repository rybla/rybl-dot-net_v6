module Blog.Paths
  ( dirPath_input,
    dirPath_post,
    listFilePaths_post,
  )
where

import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>))

dirPath_input :: FilePath
dirPath_input = "input"

dirPath_post :: FilePath
dirPath_post = dirPath_input </> "post"

listFilePaths_post :: IO [FilePath]
listFilePaths_post =
  listDirectory dirPath_post
    <&> (((dirPath_post </>) <$>) . filter (".md" `isSuffixOf`))
