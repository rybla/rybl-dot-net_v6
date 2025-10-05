module Blog.Print.ReferencesGraph where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Print.Common
import Blog.Utility
import Control.Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printReferencesGraph ::
  (MonadIO m, MonadError Doc m, MonadState env m) =>
  m ()
printReferencesGraph = do
  -- templateText <- TextIO.readFile (Paths.offlineSite.template.here </> ("page" & toHtmlFileName)) & liftIO

  -- contentHtml <-
  --   Pandoc.writeHtml5String
  --     (commonWriterOptions mempty mempty)
  --     (error "doc")
  --     & Pandoc.lensPandocM _1

  -- pageTemplate <-
  --   Pandoc.compileTemplate mempty templateText
  --     & unBlogTemplateMonad
  --     >>= fromEither (("compileTemplate:" <+>) . text)

  -- pageHtml <- do
  --   vars <-
  --     Aeson.parseEither
  --       Aeson.parseJSON
  --       ( Aeson.object
  --           [ ("title", "References Graph" & Aeson.toJSON),
  --             ("content", contentHtml & Aeson.toJSON)
  --           ]
  --       )
  --       & fromEither (("Error when parsing template variables JSON:" <+>) . text)
  --   Pandoc.writeHtml5String
  --     (commonWriterOptions (Just pageTemplate) vars)
  --     (error "doc")
  --     & Pandoc.lensPandocM _1

  -- TextIO.writeFile (page._pageId & toPageFilePath) pageHtml & liftIO
  pure ()
