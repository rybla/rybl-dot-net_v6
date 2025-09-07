{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Spec.AesonUri (test) where

import Blog.Common (UriReference (..))
import Data.Aeson
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Network.URI

data WrappedUri = WrappedUri {wrappedUri :: URI}
  deriving (Show, Generic, ToJSON, FromJSON)

test :: IO ()
test = do
  -- let wu = WrappedUri . fromJust $ parseURIReference "/google"
  -- putStrLn $ "wu     = " ++ show wu
  -- let wuJson = encode wu
  -- putStrLn $ "wuJson = " ++ show wuJson
  -- let Just wu' = decode @WrappedUri wuJson
  -- putStrLn $ "wu'    = " ++ show wu'

  let ur = UriReference . fromJust $ parseURIReference "/google"
  putStrLn $ "ur     = " ++ show ur
  let urJson = encode ur
  putStrLn $ "urJson = " ++ show urJson
  let ur' = decode @UriReference urJson
  putStrLn $ "ur'    = " ++ show ur'

  return ()
