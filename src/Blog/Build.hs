{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Build (main) where

import Blog.Common
import Blog.Env
import qualified Blog.Parse.Page as Parse.Page
import qualified Blog.Parse.Post as Parse.Post
import qualified Blog.Paths as Paths
import qualified Blog.Print.Index as Print.Index
import qualified Blog.Print.Page as Print.Page
import qualified Blog.Print.Post as Print.Post
import qualified Blog.Process.Post as Process.Post
import Blog.Utility
import Control.Category ((>>>))
import Control.Lens hiding ((<.>))
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, evalStateT)
import Data.Foldable (traverse_)
import qualified Data.Text.IO as TextIO
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
  -- parse pages
  pages <-
    listDirectory Paths.offline.page_markdown.here
      & liftIO
      <&> foldMap ((^? suffixed ".md") >>> maybe [] (return . PageId))
      >>= traverse \pageId -> do
        pageText <- TextIO.readFile (pageId & toPageMarkdownFilePath) & liftIO
        Parse.Page.parsePage pageId pageText

  -- parse posts
  posts <-
    listDirectory Paths.offline.post_markdown.here
      & liftIO
      <&> foldMap ((^? suffixed ".md") >>> maybe [] (return . PostId))
      >>= traverse \postId -> do
        postText <- TextIO.readFile (postId & toPostMarkdownFilePath) & liftIO
        Parse.Post.parsePost outLinks inLinks postId postText

  -- process posts
  posts :: [Post] <-
    posts & traverse \post -> do
      fmap (^. _1) $ execIsoStateT (pairIso post) do
        Process.Post.processPost (_2 . manager) (_2 . outLinks) (_2 . inLinks) _1

  -- print index
  Print.Index.printIndex posts

  -- print pages
  pages & traverse_ \page -> do
    Print.Page.printPage page

  -- print posts
  posts & traverse_ \post -> do
    Print.Post.printPost post
