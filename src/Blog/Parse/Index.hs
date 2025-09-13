{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Blog.Parse.Index where

import Blog.Common
import qualified Blog.Pandoc as Pandoc
import qualified Blog.Paths as Paths
import Blog.Utility
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadIO)
import qualified Data.Text as Text
import System.FilePath ((</>))
import qualified Text.Pandoc as Pandoc
import Text.PrettyPrint.HughesPJClass (Doc)

parse ::
  (MonadIO m, MonadError Doc m, MonadState env m) =>
  [Post] ->
  m Pandoc.Pandoc
parse posts = do
  postCards <- posts & traverse parsePostCard
  return $
    Pandoc.Pandoc
      mempty
      ( concat
          [ [Pandoc.Header 1 mempty [Pandoc.Str "Index"]],
            postCards
          ]
      )

parsePostCard :: (Monad m) => Post -> m Pandoc.Block
parsePostCard post = do
  return
    $ Pandoc.Div
      (mempty & Pandoc.attrClasses %~ (["PostCard"] ++))
      . concat
    $ [ [ Pandoc.Div
            (mempty & Pandoc.attrClasses %~ (["title"] ++))
            [ Pandoc.Plain
                [ Pandoc.Link
                    mempty
                    [Pandoc.Str post.postTitle]
                    (Text.pack (Paths.online.post.here </> undefined post.postId), mempty)
                ]
            ]
        ],
        [ Pandoc.Div
            (mempty & Pandoc.attrClasses %~ (["abstract"] ++))
            [ Pandoc.Plain [Pandoc.Str abstract]
            ]
        | abstract <- post.postAbstract & refold
        ]
      ]
