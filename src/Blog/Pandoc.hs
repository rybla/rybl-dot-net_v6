{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Pandoc where

import Blog.Utility (renderText)
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
      Left err -> throwError . text . show $ err
      Right a -> return a

pandocMeta :: Pandoc -> Meta
pandocMeta (Pandoc m _) = m

getMetaValue :: (MonadError Doc m) => String -> Pandoc -> m MetaValue
getMetaValue key =
  pandocMeta
    >>> lookupMeta (Text.pack key)
    >>> maybe (throwError $ "Error when extracting metadata from parsed document: missing key:" <+> doubleQuotes (text key)) return

getMetaValueList :: (MonadError Doc m) => String -> Pandoc -> m [MetaValue]
getMetaValueList key =
  pandocMeta
    >>> lookupMeta (Text.pack key)
    >>> maybe
      (throwError $ "Error when extracting metadata from parsed document: missing key:" <+> doubleQuotes (text key))
      \case
        MetaList vs -> return vs
        val -> throwError $ "Error when extracting metadata from parsed document: expected value of key" <+> doubleQuotes (text (show key)) <+> "to be a list but it was actually:" <+> text (show val)

throwPandocError :: (PandocMonad m) => Doc -> m a
throwPandocError = throwError . PandocAppError . renderText

fromDocError :: (PandocMonad m) => ExceptT Doc m a -> m a
fromDocError = runExceptT >=> either throwPandocError return

pandocBlocks :: Pandoc -> [Block]
pandocBlocks (Pandoc _ blocks) = blocks
