module Blog.Print.Page where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Print.Common
import Blog.Utility
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Default (def)
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc, text, (<+>))

printPage :: (MonadIO m, MonadError Doc m, MonadState env m) => Page -> m ()
printPage page = evalIsoStateT (pairIso def) do
  templateText <- TextIO.readFile (Paths.offline.template.here </> ("page" & toHtmlFileName)) & liftIO

  contentHtml <-
    Pandoc.writeHtml5String
      Pandoc.def
        { Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
        }
      page._pageDoc
      & Pandoc.lensPandocM _1

  pageTemplate <-
    Pandoc.compileTemplate mempty templateText
      & unBlogTemplateMonad
      >>= fromEither (("compileTemplate:" <+>) . text)

  pageHtml <- do
    vars <-
      Aeson.parseEither
        Aeson.parseJSON
        ( Aeson.object
            [ ("title", page._pageTitle & Aeson.toJSON),
              ("content", contentHtml & Aeson.toJSON)
            ]
        )
        & fromEither (("Error when parsing template variables JSON:" <+>) . text)
    Pandoc.writeHtml5String
      Pandoc.def
        { Pandoc.writerTemplate = Just pageTemplate,
          Pandoc.writerVariables = vars,
          Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
        }
      (page ^. pageDoc)
      & Pandoc.lensPandocM _1

  TextIO.writeFile (page._pageId & toPageFilePath) pageHtml & liftIO
