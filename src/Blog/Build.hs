{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blog.Build (main) where

import Blog.Common
import Blog.Env
import qualified Blog.Parse.Post as Parse.Post
import qualified Blog.Paths as Paths
import qualified Blog.Print.Post as Print.Post
import qualified Blog.Process.Post as Process.Post
import Blog.Utility (logM, showDoc)
import Control.Category ((>>>))
import Control.Lens hiding ((<.>))
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, evalStateT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable (traverse_)
import Service.Favicon.Favicone ()
import Service.Preview.Placeholder ()
import System.Directory (listDirectory)
import Text.PrettyPrint.HughesPJClass

main :: IO ()
main = do
  env <- newEnv
  main' & (`evalStateT` env) & runExceptT >>= \case
    Left err -> error . render $ "Error:" <+> text (show err)
    Right it -> return it

main' :: forall m. (MonadIO m, MonadError Doc m, MonadState Env m) => m ()
main' = do
  -- TODO: skip saving encoded data; just keep it in memory
  -- parse posts
  parsePostEnv <-
    listDirectory Paths.offline.post_markdown.here
      & liftIO
      <&> foldMap ((^? suffixed ".md") >>> maybe [] (return . PostId))
      >>= traverse_ \postId ->
        Parse.Post.parse outLinks inLinks postId
          >>= \post -> ByteString.writeFile (postId & toPostDataFilePath) (post & postDoc & Aeson.encode) & liftIO

  logM "main" $ "parsePostEnv =" <+> showDoc parsePostEnv

  -- process posts
  _processPostEnv <-
    listDirectory Paths.offline.post_data.here
      & liftIO
      <&> foldMap ((^? suffixed ".json") >>> maybe [] (return . PostId))
      >>= traverse_ \postId ->
        Process.Post.process manager outLinks inLinks postId
          -- >>= Paths.writePostData postId
          -- >>= \post -> ByteString.writeFile (postId & toPostFilePath) (post & _) & liftIO
          >>= \post -> ByteString.writeFile (postId & toPostDataFilePath) (post & postDoc & Aeson.encode) & liftIO

  -- print posts
  listDirectory Paths.offline.post_data.here
    & liftIO
    <&> foldMap ((^? suffixed ".json") >>> maybe [] (return . PostId))
    >>= traverse_ \postId -> do
      Print.Post.printPost postId
