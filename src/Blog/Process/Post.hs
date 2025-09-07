{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blog.Process.Post where

import Blog.Common
import Blog.Parse.Post (outLinks, inLinks)
import qualified Blog.Parse.Post as Parse.Post
import qualified Blog.Paths as Paths
import Blog.Process.Common
import Blog.Utility (parseUriReferenceM)
import Control.Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State (MonadIO, MonadState, gets)
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Client as Network
import Service.Favicon (FaviconService)
import System.FilePath ((</>))
import Text.Pandoc (Pandoc)
import Text.PrettyPrint.HughesPJClass (Doc)

data Env = Env
  { _parseEnv :: Parse.Post.Env,
    _manager :: Network.Manager
  }

newEnv :: Parse.Post.Env -> Network.Manager -> Env
newEnv _parseEnv _manager =
  Env
    { _parseEnv,
      _manager
    }

makeLenses ''Env

processPost :: forall fs m. (FaviconService fs, MonadError Doc m, MonadState Env m, MonadIO m) => PostId -> m Pandoc
processPost postId = do
  postUri <- parseUriReferenceM ("" </> (postId & unPostId & Paths.toHtmlFileName))

  post <- Paths.readPostData postId

  post <- do
    ols <- gets (^. parseEnv . outLinks . at postUri . to (Maybe.fromMaybe []))
    post & addReferencesSection ols

  post <- do
    ils <- gets (^. parseEnv . inLinks . at postUri . to (Maybe.fromMaybe []))
    post & addCitationsSection ils

  mngr <- gets (^. manager)
  post <- post & addLinkFavicons @fs mngr

  post <- post & addTableOfContents

  return post
