module Service.Preview.Placeholder where

import Service.Preview

data PlaceholderPreviewService

instance PreviewService PlaceholderPreviewService where
  previewUri _ uri manager = do
    return
      Preview
        { _title = show uri,
          _description = "Placeholder description for " ++ show uri
        }
