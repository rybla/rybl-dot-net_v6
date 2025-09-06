{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Pandoc where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.State.Lazy as StateLazy
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text as Text
import Text.Pandoc
import Text.PrettyPrint.HughesPJClass

runPandocM :: (MonadError Doc m2, MonadIO m2) => PandocIO a -> m2 a
runPandocM =
  unPandocIO
    >>> runExceptT
    >>> (`StateLazy.evalStateT` def)
    >>> liftIO
    >=> \case
      Left (PandocAppError msg) -> throwError . text . ("PandocAppError: " ++) . Text.unpack $ msg
      Left err -> throwError . text . show $ err
      Right a -> return a

pandocMeta :: Pandoc -> Meta
pandocMeta (Pandoc m _) = m

getMetaValue :: (PandocMonad m) => String -> Pandoc -> m MetaValue
getMetaValue key =
  pandocMeta
    >>> lookupMeta (Text.pack key)
    >>> maybe (throwError . PandocCouldNotFindMetadataFileError . Text.pack $ "Error when extracting metadata from parsed document: missing key: \"" ++ key ++ "\"") return

getMetaValueList :: (PandocMonad m) => String -> Pandoc -> m [MetaValue]
getMetaValueList key =
  pandocMeta
    >>> lookupMeta (Text.pack key)
    >>> maybe
      (throwError . PandocCouldNotFindMetadataFileError . Text.pack $ "Error when extracting metadata from parsed document: missing key: \"" ++ key ++ "\"")
      \case
        MetaList vs -> return vs
        v -> throwError . PandocCouldNotFindMetadataFileError . Text.pack $ "Error when extracting metadata from parsed document: expected value of key \"" ++ show v ++ "\" to be a list but it was actually: " ++ show v

throwPandocError :: (PandocMonad m) => Doc -> m a
throwPandocError = throwError . PandocAppError . Text.pack . render

fromDocError :: (PandocMonad m) => ExceptT Doc m a -> m a
fromDocError = runExceptT >=> either throwPandocError return

pandocBlocks :: Pandoc -> [Block]
pandocBlocks (Pandoc _ blocks) = blocks
