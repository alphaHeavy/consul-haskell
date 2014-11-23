{-# LANGUAGE OverloadedStrings #-}

module Network.Consul.Internal (
    getDatacenters
  , getKey
  , putKey) where

import Data.Aeson (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Consul.Types
import Network.HTTP.Client
import Network.Socket (PortNumber(..))

{- Key Value Store -}
getKey :: Manager -> Text -> PortNumber -> KeyValueRequest -> IO (Maybe KeyValue)
getKey manager hostname (PortNum portNumber) request = do
  initReq <- parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", kvrKey request]
  withResponse initReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ listToMaybe =<< (decode $ BL.fromStrict body)

putKey :: Manager -> Text -> PortNumber -> KeyValuePut -> IO Text
putKey manager hostname (PortNum portNumber) request = do
  initReq <- parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", kvpKey request]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ kvpValue request}
  withResponse httpReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ TE.decodeUtf8 body

{- Catalog -}
getDatacenters :: Manager -> Text -> PortNumber -> IO [Datacenter]
getDatacenters manager hostname (PortNum portNumber) = do
  initReq <- parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/catalog/datacenters/"]
  withResponse initReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let val = (decode $ BL.fromStrict body)
    case val of
      Just x -> return x
      Nothing -> return []

