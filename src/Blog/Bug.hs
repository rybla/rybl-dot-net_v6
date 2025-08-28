{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Bug where

import Control.Monad.Except (MonadError, throwError)
import Text.PrettyPrint.HughesPJClass (Doc)

data Bug = Bug Doc

type MonadBug = MonadError Bug

throwBug :: (MonadBug m) => Doc -> m a
throwBug = throwError . Bug
