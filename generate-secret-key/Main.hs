module Main where

import Blog.Common
import qualified Blog.Config as Config
import Blog.Utility
import qualified Crypto.PubKey.Ed25519 as Crypto
import qualified Data.ByteString as ByteString
import Text.PrettyPrint.HughesPJClass (text, (<+>))

main :: IO ()
main = do
  logM "generate-secret-key" $ "Generating new secret key"
  sk <- Crypto.generateSecretKey
  ByteString.writeFile secretKeyFilePath $ fromByteArrayAccessToByteString sk
  logM "generate-secret-key" $ "Wrote new secret key to file:" <+> text secretKeyFilePath
  let pk = Crypto.toPublic sk
  writeFile publicKeyFilePath $ showByteArrayAsBase64 pk
  logM "generate-secret-key" $ "Wrote new public key to file:" <+> text publicKeyFilePath
