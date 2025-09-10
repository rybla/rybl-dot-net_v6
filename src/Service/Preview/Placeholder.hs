{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Service.Preview.Placeholder () where

import Service.Preview

instance PreviewService where
  previewUri uri _manager = do
    return
      Preview
        { title = show uri,
          description = "Placeholder description for " ++ show uri
        }
