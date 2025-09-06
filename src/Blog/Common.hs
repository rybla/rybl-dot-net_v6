{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Common where

import Text.PrettyPrint.HughesPJClass (Pretty, brackets, hcat, pPrint, text)

newtype PostId = PostId {unPostId :: String}
  deriving (Show, Eq, Ord)

instance Pretty PostId where
  pPrint (PostId pid) = hcat ["Post", brackets (text pid)]
