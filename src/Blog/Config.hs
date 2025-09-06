{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Config where

data Mode = Development | Production
  deriving (Show, Eq)

#if defined(DEVELOPMENT)
mode :: Mode
mode = Development
#else
mode :: Mode
mode = Production
#endif
