{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Config where

baseTitle :: String
baseTitle = "rybl.net"

baseDescription :: String
baseDescription = "TODO: baseDescription"

data Mode = Development | Production
  deriving (Show, Eq)

#if defined(DEVELOPMENT)
mode :: Mode
mode = Development
#else
mode :: Mode
mode = Production
#endif
