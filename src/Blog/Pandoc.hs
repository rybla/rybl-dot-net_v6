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
import Text.PrettyPrint.HughesPJClass hiding (Str)

runPandocM :: (MonadError Doc m2, MonadIO m2) => PandocIO a -> m2 a
runPandocM =
  unPandocIO
    >>> runExceptT
    >>> ( `StateLazy.evalStateT`
            def
              { stUserDataDir = Just Paths.offlineSite.template.here
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

_pandocMeta :: Lens' Pandoc Meta
_pandocMeta f (Pandoc meta blocks) = (`Pandoc` blocks) <$> f meta

pandocBlocks :: Pandoc -> [Block]
pandocBlocks (Pandoc _ blocks) = blocks

_pandocBlocks :: Lens' Pandoc [Block]
_pandocBlocks f (Pandoc meta blocks) = Pandoc meta <$> f blocks

getMetaValueMaybe :: String -> Pandoc -> Maybe MetaValue
getMetaValueMaybe key = pandocMeta >>> lookupMeta (Text.pack key)

getMetaValue :: (MonadError Doc m) => String -> Pandoc -> m MetaValue
getMetaValue key =
  getMetaValueMaybe key
    >>> maybe (throwError $ "Error when extracting metadata from parsed document: missing key:" <+> doubleQuotes (text key)) return

getMetaValueSuchThat :: (MonadError Doc m) => (MetaValue -> Either Doc a) -> String -> Pandoc -> m a
getMetaValueSuchThat f key =
  getMetaValue key
    >=> \v -> case f v of
      Left err ->
        throwError $
          "Error when extracting metadata from parsed document: at key"
            <+> doubleQuotes (text key)
            <+> ( nest 4 $
                    vcat
                      [ err,
                        "v =" <+> text (show v)
                      ]
                )
      Right a -> return a

getMetaValueMaybeSuchThat :: (MonadError Doc m) => (MetaValue -> Either Doc a) -> String -> Pandoc -> m (Maybe a)
getMetaValueMaybeSuchThat f key =
  getMetaValueMaybe key >>> \case
    Nothing -> pure Nothing
    Just v -> case f v of
      Left err ->
        throwError $
          "Error when extracting metadata from parsed document: at key"
            <+> doubleQuotes (text key)
            <+> ( nest 4 $
                    vcat
                      [ err,
                        "v =" <+> text (show v)
                      ]
                )
      Right a -> pure (Just a)

fromMetaListString :: MetaValue -> Either Doc [Text]
fromMetaListString = \case
  MetaList vs -> vs & traverse (left' ("in list," <+>) . fromMetaString)
  _ -> Left "expected list of string"

fromMetaString :: MetaValue -> Either Doc Text
fromMetaString = \case
  MetaString s -> pure s
  MetaInlines is -> pure $ Pandoc.stringify is
  _ -> throwError "expected string"

fromMetaInlines :: MetaValue -> Either Doc [Inline]
fromMetaInlines = \case
  MetaInlines is -> pure is
  _ -> throwError "expected inlines"

fromMetaBlocks :: MetaValue -> Either Doc [Block]
fromMetaBlocks = \case
  MetaString s -> pure [Para [Str s]]
  MetaInlines is -> pure [Para is]
  MetaBlocks bs -> pure bs
  _ -> throwError "expected blocks"

throwPandocError :: (PandocMonad m) => Doc -> m a
throwPandocError = throwError . PandocAppError . renderText

fromDocError :: (PandocMonad m) => ExceptT Doc m a -> m a
fromDocError = runExceptT >=> either throwPandocError return

attrId :: Lens' Attr Text
attrId = _1

attrClasses :: Lens' Attr [Text]
attrClasses = _2

attrData :: Lens' Attr [(Text, Text)]
attrData = _3
