{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Blog.Secret where

import Blog.Common
import Blog.Utility
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Error as Crypto
import qualified Crypto.PubKey.Ed25519 as Crypto
import qualified Data.ByteString as ByteString
import qualified Language.Haskell.TH.Syntax as Syntax

secretKey :: Crypto.SecretKey
secretKey =
  $( do
       bs <- liftIO $ ByteString.readFile secretKeyFilePath
       [|Crypto.throwCryptoError $ Crypto.secretKey $ $(Syntax.lift bs)|]
   )

publicKey :: Crypto.PublicKey
publicKey = Crypto.toPublic secretKey

generateSecretKey :: IO ()
generateSecretKey = do
  sk <- Crypto.generateSecretKey
  -- secret key is stored as a bytestring
  ByteString.writeFile secretKeyFilePath $ fromByteArrayAccessToByteString sk
  let pk = Crypto.toPublic sk
  -- public key is stored as a readable base 64 number
  writeFile publicKeyFilePath $ show $ fromByteArrayAccessToByteString pk
