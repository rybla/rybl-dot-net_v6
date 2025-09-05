{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Utility where

import Control.Monad.Except (MonadError, throwError)
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Network.URI (URI)
import qualified Network.URI as URI
import qualified Network.URI.Encode as URI
import qualified System.FilePath as FilePath
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
-- > extractUriHost "https://example.com#id?query=string" == "example.com"
extractUriHost :: URI -> String
extractUriHost uri =
  URI.uriAuthToString mempty (URI.uriAuthority uri) ""
    & List.stripPrefix "//"
    & fromJust

extractUriPath :: URI -> String
extractUriPath uri = extractUriHost uri ++ URI.uriPath uri ++ URI.uriFragment uri

fromMaybe :: (MonadError Doc m) => Doc -> Maybe a -> m a
fromMaybe msg = maybe (throwError msg) return

fromEither :: (MonadError Doc m) => (e -> Doc) -> Either e a -> m a
fromEither mkMsg = either (throwError . mkMsg) return

makeValidIdent :: String -> String
makeValidIdent = FilePath.makeValid . URI.encode

markdownLink :: String -> String -> String
markdownLink label url = "[" ++ label ++ "](" ++ url ++ ")"

markdownImage :: String -> String -> String
markdownImage label url = "![" ++ label ++ "](" ++ url ++ ")"
