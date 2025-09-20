{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Config where
import Data.String (IsString)

baseTitle :: String
baseTitle = "rybl.net"

baseDescription :: String
baseDescription = "TODO: baseDescription"

-- TODO: move these to Paths

secretKeyFilePath :: IsString s => s
secretKeyFilePath = "secret/main_ed25519"

publicKeyFilePath :: IsString s => s
publicKeyFilePath = "site/key/main_ed25519.pub.txt"

publicKeyUri :: IsString s => s
publicKeyUri = "/key/main_ed25519.pub.txt"
