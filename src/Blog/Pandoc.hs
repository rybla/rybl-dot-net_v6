{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Pandoc where

import qualified Blog.Paths as Paths
import Blog.Utility (renderText)
import Control.Category ((>>>))
import Control.Lens
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get)
import qualified Control.Monad.State.Lazy as StateLazy
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Pandoc
import qualified Text.Pandoc.Shared as Pandoc
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

lensPandocM ::
  (MonadError Doc m2, MonadIO m2, MonadState env m2) =>
  Lens' env CommonState -> PandocIO a -> m2 a
lensPandocM l m = do
  env <- get
  m
    & unPandocIO
    & runExceptT
    & (`StateLazy.runStateT` (env ^. l))
    & liftIO
    >>= \case
      (Left err, _) -> throwError $ text $ show err
      (Right a, env') -> do
        l .= env'
        return a

pandocMeta :: Pandoc -> Meta
pandocMeta (Pandoc m _) = m

getMetaValueMaybe :: String -> Pandoc -> Maybe MetaValue
getMetaValueMaybe key = pandocMeta >>> lookupMeta (Text.pack key)

getMetaValue :: (MonadError Doc m) => String -> Pandoc -> m MetaValue
getMetaValue key =
  getMetaValueMaybe key
    >>> maybe (throwError $ "Error when extracting metadata from parsed document: missing key:" <+> doubleQuotes (text key)) return

getMetaValueSuchThat :: (MonadError Doc m) => (String, MetaValue -> Maybe a) -> String -> Pandoc -> m a
getMetaValueSuchThat (label, f) key =
  getMetaValue key
    >=> \v -> case f v of
      Nothing -> throwError $ "Error when extracting metadata from parsed document: expected value of key" <+> doubleQuotes (text key) <+> "to be a" <+> text label <+> "but it was actually:" <+> text (show v)
      Just a -> return a

getMetaValueMaybeSuchThat :: (MonadError Doc m) => (String, MetaValue -> Maybe a) -> String -> Pandoc -> m (Maybe a)
getMetaValueMaybeSuchThat (label, f) key =
  getMetaValueMaybe key >>> \case
    Nothing -> pure Nothing
    Just v -> case f v of
      Nothing -> throwError $ "Error when extracting metadata from parsed document: expected value of key" <+> doubleQuotes (text key) <+> "to be a" <+> text label <+> "but it was actually:" <+> text (show v)
      Just a -> pure (Just a)

fromMetaListString :: (String, MetaValue -> Maybe [Text])
fromMetaListString =
  ( "list of string",
    \case
      MetaList vs ->
        vs & traverse (snd fromMetaString)
      _ -> Nothing
  )

fromMetaString :: (String, MetaValue -> Maybe Text)
fromMetaString =
  ( "string",
    \case
      MetaString s -> Just s
      MetaInlines is -> Just $ Pandoc.stringify is
      _ -> Nothing
  )

fromMetaInlines :: (String, MetaValue -> Maybe [Inline])
fromMetaInlines =
  ( "inlines",
    \case
      MetaInlines is -> Just is
      _ -> Nothing
  )

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
