{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Consul (getKeyInternal) where

import Data.Aeson (decode,parseJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Consul.Types
import Network.HTTP.Client
import Network.HTTP.Client.Internal

getKeyInternal :: Manager -> KeyValueRequest -> IO (Maybe KeyValue)
getKeyInternal manager request = do
  initReq <- parseUrl $ "http://localhost:8500/v1/kv/" ++ (T.unpack $ kvrKey request)
  withResponse initReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    print body
    let val :: Maybe [KeyValue] = decode $ BL.fromStrict body
    return $ listToMaybe =<< val

putKeyInternal :: Manager -> KeyValuePut -> IO Text
putKeyInternal manager request = do
  initReq <- parseUrl $ "http://localhost:8500/v1/kv/" ++ (T.unpack $ kvpKey request)
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ kvpValue request}
  withResponse httpReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ TE.decodeUtf8 body

