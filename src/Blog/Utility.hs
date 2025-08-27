{-# LANGUAGE OverloadedStrings #-}

module Blog.Utility (prettyPandoc, extractHost) where

import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Network.URI (URI, uriAuthToString, uriAuthority)
import Text.Pandoc
import Text.PrettyPrint.HughesPJClass

prettyPandoc :: Pandoc -> Doc
prettyPandoc (Pandoc meta blocks) =
  vcat $
    [ hang "meta:" 4 . vcat $
        fmap ((\(k, v) -> text (show k) <+> ":=" <+> text (show v))) . Map.toList . unMeta $
          meta,
      hang "blocks:" 4 . vcat $
        fmap (text . show) $
          blocks
    ]

-- | Extract the host from a URI. Example:
--
-- > extractHost "https://example.com?q=hello" == "example.com"
extractHost :: URI -> String
extractHost uri =
  uriAuthToString mempty (uriAuthority uri) ""
    & List.stripPrefix "//"
    & fromJust
