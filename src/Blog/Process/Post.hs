{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Process.Post where

import Blog.Common
import qualified Blog.Parse.Post as Parse.Post
import Control.Lens
import Control.Monad.State (MonadState)
import qualified Network.HTTP.Client as Network
import Text.Pandoc (Pandoc)

data Env = Env
  { parseEnv :: Parse.Post.Env,
    manager :: Network.Manager
  }

newEnv :: Parse.Post.Env -> Network.Manager -> Env
newEnv parseEnv manager =
  Env
    { parseEnv,
      manager
    }

makeLenses ''Env

processPost :: (MonadState Env m) => PostId -> m Pandoc
processPost postId = undefined
