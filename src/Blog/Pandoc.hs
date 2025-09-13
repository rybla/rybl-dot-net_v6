{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Pandoc where

import qualified Blog.Paths as Paths
import Blog.Utility (renderText)
import Control.Category ((>>>))
import Control.Lens
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.State.Lazy as StateLazy
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Pandoc
import Text.PrettyPrint.HughesPJClass

runPandocM :: (MonadError Doc m2, MonadIO m2) => PandocIO a -> m2 a
runPandocM =
  unPandocIO
    >>> runExceptT
    >>> ( `StateLazy.evalStateT`
            def
              { stUserDataDir = Just Paths.offline.template.here
              }
        )
    >>> liftIO
    >=> \case
      Left err -> throwError . text . show $ err
      Right a -> return a

pandocMeta :: Pandoc -> Meta
pandocMeta (Pandoc m _) = m

getMetaValueMaybe :: String -> Pandoc -> Maybe MetaValue
getMetaValueMaybe key = pandocMeta >>> lookupMeta (Text.pack key)

getMetaValue :: (MonadError Doc m) => String -> Pandoc -> m MetaValue
getMetaValue key =
  getMetaValueMaybe key
    >>> maybe (throwError $ "Error when extracting metadata from parsed document: missing key:" <+> doubleQuotes (text key)) return

getMetaValueSuchThat :: (MonadError Doc m) => String -> (MetaValue -> Maybe a) -> String -> Pandoc -> m a
getMetaValueSuchThat label f key =
  getMetaValue key
    >=> \v -> case f v of
      Nothing -> throwError $ "Error when extracting metadata from parsed document: expected value of key" <+> doubleQuotes (text (show key)) <+> "to be a" <+> text label <+> "but it was actually:" <+> text (show v)
      Just a -> return a

getMetaValueListString :: (MonadError Doc m) => String -> Pandoc -> m [Text]
getMetaValueListString = getMetaValueSuchThat "list of strings" \case
  MetaList vs ->
    vs & traverse \case
      MetaString s -> Just s
      _ -> Nothing
  _ -> Nothing

getMetaValueString :: (MonadError Doc m) => String -> Pandoc -> m Text
getMetaValueString = getMetaValueSuchThat "string" \case
  MetaString s -> Just s
  _ -> Nothing

throwPandocError :: (PandocMonad m) => Doc -> m a
throwPandocError = throwError . PandocAppError . renderText

fromDocError :: (PandocMonad m) => ExceptT Doc m a -> m a
fromDocError = runExceptT >=> either throwPandocError return

pandocBlocks :: Pandoc -> [Block]
pandocBlocks (Pandoc _ blocks) = blocks

attrId :: Lens' Attr Text
attrId = _1

attrClasses :: Lens' Attr [Text]
attrClasses = _2

attrData :: Lens' Attr [(Text, Text)]
attrData = _3
