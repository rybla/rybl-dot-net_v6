{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blog.Build (main) where

import Blog.Common
import qualified Blog.Parse.Post as Parse.Post
import qualified Blog.Paths as Paths
import qualified Blog.Print.Post as Print.Post
import qualified Blog.Process.Post as Process.Post
import Control.Category ((>>>))
import Control.Lens hiding ((<.>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Foldable (traverse_)
import qualified Network.HTTP.Client as Network
import qualified Network.HTTP.Client.TLS as Network
import Service.Favicon.Favicone (FaviconeService)
import System.Directory (listDirectory)
import Text.PrettyPrint.HughesPJClass

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error . render $ "Error:" <+> text (show err)
    Right it -> return it

main' :: ExceptT Doc IO ()
main' = do
  manager <- Network.newManager Network.tlsManagerSettings & liftIO

  -- parse posts
  parsePostEnv <-
    (`execStateT` Parse.Post.newEnv) $
      listDirectory Paths.offline.post_markdown.here
        & liftIO
        <&> foldMap ((^? suffixed ".md") >>> maybe [] (return . PostId))
        >>= traverse_ \postId ->
          Parse.Post.parsePost postId
            >>= Paths.writePostData postId

  -- process posts
  _processPostEnv <-
    (`execStateT` Process.Post.newEnv parsePostEnv manager) $
      listDirectory Paths.offline.post_data.here
        & liftIO
        <&> foldMap ((^? suffixed ".json") >>> maybe [] (return . PostId))
        >>= traverse_ \postId ->
          Process.Post.processPost @FaviconeService postId
            >>= Paths.writePostData postId

  -- print posts
  listDirectory Paths.offline.post_data.here
    & liftIO
    <&> foldMap ((^? suffixed ".json") >>> maybe [] (return . PostId))
    >>= traverse_ \postId -> do
      Print.Post.printPost postId
