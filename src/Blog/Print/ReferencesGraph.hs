{-# LANGUAGE RankNTypes #-}

module Blog.Print.ReferencesGraph where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Print.Common
import Blog.Utility
import Control.Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, gets)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Default (def)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Network.URI (URI)
import System.FilePath ((</>))
import Text.Pandoc (Pandoc (..))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printReferencesGraph ::
  (MonadIO m, MonadError Doc m, MonadState env m) =>
  Lens' env (Map URI Text) ->
  Lens' env (Map URI [Link]) ->
  m ()
printReferencesGraph uriLabels outLinks = evalIsoStateT (pairIso def) do
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

  let body = Pandoc mempty []

  (nodes, edges) <- do
    uls <- gets (^. _2 . uriLabels)
    ols <- gets (^. _2 . outLinks)

    let dqs t = "\"" <> t <> "\""

    let nodes :: Text
        edges :: Text
        (nodes, edges) =
          zipMapsWithDefault
            (\linkUri _links -> linkUri & showText)
            (\_linkUri _label -> [])
            uls
            ols
            & Map.toList
            & map
              ( \(uri, (label, links)) ->
                  let uriText = uri & show & show & Text.pack
                   in ( "{ id: " <> uriText <> ", label: " <> (label & show & Text.pack) <> ", url: " <> uriText <> ", shape: " <> dqs "box" <> " }",
                        ["{ from: " <> uriText <> ", to: " <> (link.linkUri & show & show & Text.pack) <> " }" | link <- links]
                      )
              )
            & unzip
            & bimap
              ((\s -> "[" <> s <> "]") . Text.intercalate ", ")
              ((\s -> "[" <> s <> "]") . Text.intercalate ", " . concat)

    pure (nodes, edges)

  pageHtml <- do
    vars <-
      Aeson.parseEither
        Aeson.parseJSON
        ( Aeson.object
            [ ("title", "References Graph"),
              ("nodes", nodes & Aeson.toJSON),
              ("edges", edges & Aeson.toJSON)
            ]
        )
        & fromEither (("Error when parsing template variables JSON:" <+>) . text)
    Pandoc.writeHtml5String
      (commonWriterOptions (Just pageTemplate) vars)
      body
      & Pandoc.lensPandocM _1

  TextIO.writeFile (Paths.offlineSite.page.here </> ("references-graph" & toHtmlFileName)) pageHtml & liftIO
