{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Blog.Secret where

import qualified Blog.Config as Config
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Error as Crypto
import qualified Crypto.PubKey.Ed25519 as Crypto
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as ByteString
import qualified Language.Haskell.TH.Syntax as Syntax

secretKey :: Crypto.SecretKey
secretKey =
  $( do
       bs <- liftIO $ ByteString.readFile Config.secretKeyFilePath
       [|Crypto.throwCryptoError $ Crypto.secretKey $ $(Syntax.lift bs)|]
   )

publicKey :: Crypto.PublicKey
publicKey = Crypto.toPublic secretKey

generateSecretKey :: IO ()
generateSecretKey = do
  sk <- Crypto.generateSecretKey
  ByteString.writeFile Config.secretKeyFilePath $ ByteString.pack $ ByteArray.unpack sk
  let pk = Crypto.toPublic sk
  writeFile Config.publicKeyFilePath $ show $ ByteString.pack $ ByteArray.unpack pk
