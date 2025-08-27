{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Favicon (fetchFaviconInfo) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Service.Favicone (FaviconeResponse)

fetchFaviconInfo :: (MonadIO m, MonadError String m) => String -> m FaviconeResponse
fetchFaviconInfo = fetchFaviconInfo
