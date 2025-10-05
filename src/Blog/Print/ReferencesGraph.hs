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
import Data.Default (def)
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printReferencesGraph ::
  (MonadIO m, MonadError Doc m, MonadState env m) =>
  m ()
printReferencesGraph = evalIsoStateT (pairIso def) do
  templateText <- TextIO.readFile (Paths.offlineSite.template.here </> ("references-graph" & toHtmlFileName)) & liftIO

  pageTemplate <-
    Pandoc.compileTemplate mempty templateText
      & unBlogTemplateMonad
      >>= fromEither (("compileTemplate:" <+>) . text)

  -- TODO: create nodes and edges following this format:
  -- const nodes = new vis.DataSet([
  --     { id: 1, label: "Google", url: "https://www.google.com", shape: 'box' },
  --     { id: 2, label: "GitHub", url: "https://www.github.com", shape: 'box' }
  -- ]);
  -- const edges = new vis.DataSet([
  --     { from: 1, to: 2 },
  --     { from: 2, to: 3 },
  --     { from: 1, to: 4 },
  -- ]);

  pageHtml <- do
    vars <-
      Aeson.parseEither
        Aeson.parseJSON
        ( Aeson.object
            [ ("title", "References Graph")
            ]
        )
        & fromEither (("Error when parsing template variables JSON:" <+>) . text)
    Pandoc.writeHtml5String
      (commonWriterOptions (Just pageTemplate) vars)
      mempty
      & Pandoc.lensPandocM _1

  TextIO.writeFile (Paths.offlineSite.here </> ("references-graph" & toHtmlFileName)) pageHtml & liftIO
