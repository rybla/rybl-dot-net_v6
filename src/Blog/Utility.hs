{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Utility where

import Control.Lens
import Control.Monad (MonadPlus (..))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Control.Monad.State (MonadState, StateT, gets, put, runStateT)
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Network.URI (URI)
import qualified Network.URI as URI
import qualified Network.URI.Encode as URI
import Safe (lastMay)
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

-- | Extract the root URI from a URI. Example:
--
-- > uriRoot "https://test.example.com/seg1#id?query=string" == "https://test.example.com"
uriRoot :: URI -> URI
uriRoot uri =
  uri
    { URI.uriFragment = mempty,
      URI.uriQuery = mempty,
      URI.uriPath = mempty
    }

-- | Extract the domain from a URI. Example:
--
-- > uriDomain "https://test.example.com/seg1#id?query=string" == "example.com"
uriDomain :: URI -> String
uriDomain uri =
  uri
    & URI.uriAuthority
    & fromJust
    & URI.uriRegName
    & List.reverse
    & splitBy ('.' ==)
    & take 2
    & List.intercalate "."
    & List.reverse

-- | Extract the domain URI from a URI. Example:
--
-- > uriRoot "https://test.example.com/seg1#id?query=string" == "https://example.com"
uriDomainUri :: URI -> URI
uriDomainUri uri =
  uri
    { URI.uriFragment = mempty,
      URI.uriAuthority = URI.uriAuthority uri <&> \auth -> auth {URI.uriRegName = uriDomain uri}
    }

-- | Extract the root URI and path from a URI. Example:
--
-- > uriRoot "https://test.example.com#id?query=string" == "https://test.example.com/seg1"
uriRootAndPath :: URI -> URI
uriRootAndPath uri =
  uri
    { URI.uriFragment = mempty,
      URI.uriQuery = mempty
    }

-- | Extract the host from a URI. Example:
--
-- > uriHost "https://www.test.example.com/seg1#id?query=string" == "test.example.com"
uriHost :: URI -> String
uriHost uri =
  uri
    & URI.uriAuthority
    & maybe (error "URI doesn't have a host: " ++ show uri) URI.uriRegName
    & splitBy ('.' ==)
    & List.filter ("www" /=)
    & List.intercalate "."

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

parseUriM :: (MonadError Doc m) => String -> m URI
parseUriM str = case URI.parseURI str of
  Nothing -> throwError $ text "Invalid URI:" <+> doubleQuotes (text str)
  Just uri -> return uri

parseUriReferenceM :: (MonadError Doc m) => String -> m URI
parseUriReferenceM str = case URI.parseURIReference str of
  Nothing -> throwError $ text "Invalid URI Reference:" <+> doubleQuotes (text str)
  Just uri -> return uri

logM :: (MonadIO m) => Doc -> Doc -> m ()
logM label body = putStrLn (render (brackets label <+> nest 4 body)) & liftIO

showText :: (Show a) => a -> Text
showText = Text.pack . show

renderText :: Doc -> Text
renderText = Text.pack . render

showDoc :: (Show a) => a -> Doc
showDoc = text . show

assocList :: (Eq k) => k -> [(k, v)] -> Maybe v
assocList _ [] = Nothing
assocList k ((k', v) : kvs) = if k == k' then Just v else assocList k kvs

(%=*) :: (MonadState s m) => Lens' s a -> (a -> m a) -> m ()
l %=* f = (l .=) =<< f =<< gets (^. l)

infix 4 %=*

runIsoStateT :: (MonadState outer m) => Iso' outer inner -> StateT inner m a -> m (inner, a)
runIsoStateT i m = do
  (a, inner) <- runStateT m =<< gets (^. i)
  put (inner ^. from i)
  return (inner, a)

execIsoStateT :: (MonadState outer m) => Iso' outer inner -> StateT inner m a -> m inner
execIsoStateT i m = fmap fst $ runIsoStateT i m

pairIso :: a -> Iso' b (a, b)
pairIso a = iso (a,) snd

evalIsoStateT :: (MonadState outer m) => Iso' outer inner -> StateT inner m a -> m a
evalIsoStateT i m = fmap snd $ runIsoStateT i m

refold :: (MonadPlus f) => Maybe a -> f a
refold = foldr (mplus . pure) mzero

parseDay :: String -> Maybe Time.Day
parseDay = Time.parseTimeM @Maybe True Time.defaultTimeLocale "%Y-%-m-%-d"

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond action = cond >>= \b -> if b then action else return ()

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = go []
  where
    go acc [] = reverse $ filter (not . null) $ acc
    go acc xs =
      let (ys, zs) = span (not . f) xs
       in go (ys : acc) (drop 1 zs)

showByteArrayAsBase64 :: (ByteArrayAccess ba) => ba -> String
showByteArrayAsBase64 = C8.unpack . B16.encode . fromByteArrayAccessToByteString

fromByteArrayAccessToByteString :: (ByteArrayAccess ba) => ba -> ByteString
fromByteArrayAccessToByteString = ByteString.pack . ByteArray.unpack

filepathExtension :: FilePath -> Maybe String
filepathExtension = lastMay . splitBy ('.' ==)

textDoc :: Text -> Doc
textDoc = text . Text.unpack
