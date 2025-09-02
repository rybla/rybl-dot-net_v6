{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Pandoc where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text as Text
import Text.Pandoc
import Text.PrettyPrint.HughesPJClass

runPandocM :: (MonadError Doc m2, MonadIO m2) => PandocIO a -> m2 a
runPandocM =
  unPandocIO
    >>> runExceptT
    -- >>> (`evalStateT` def)
    -- >>> (`evalState` def)
    -- >>> either (throwError . text . show) return
    -- >>> liftIO
    >>> (`StateLazy.evalStateT` def)
    >>> liftIO
    >=> \case
      Left err -> throwError . text . show $ err
      Right a -> return a

pandocMeta :: Pandoc -> Meta
pandocMeta (Pandoc m _) = m

getMetaValue :: (PandocMonad m) => String -> Pandoc -> m MetaValue
getMetaValue key =
  pandocMeta
    >>> lookupMeta (Text.pack key)
    >>> maybe (throwError . PandocCouldNotFindMetadataFileError . Text.pack $ "Error when extracting metadata from parsed document: missing key: \"" ++ key ++ "\"") return
