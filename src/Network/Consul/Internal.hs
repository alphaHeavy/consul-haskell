{-# LANGUAGE OverloadedStrings #-}

module Network.Consul.Internal (
    deleteKey
  , getDatacenters
  , getKey
  , putKey

  --Health
  , getServiceChecks
  ) where

import Control.Monad.IO.Class
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
getKey :: MonadIO m => Manager -> Text -> PortNumber -> KeyValueRequest -> m (Maybe KeyValue)
getKey manager hostname (PortNum portNumber) request = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", kvrKey request]
  liftIO $ withResponse initReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ listToMaybe =<< (decode $ BL.fromStrict body)

putKey :: MonadIO m => Manager -> Text -> PortNumber -> KeyValuePut -> m Text
putKey manager hostname (PortNum portNumber) request = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", kvpKey request]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ kvpValue request}
  liftIO $ withResponse httpReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ TE.decodeUtf8 body


deleteKey :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m ()
deleteKey manager hostname (PortNum portNumber) key = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", key]
  let httpReq = initReq { method = "DELETE"}
  liftIO $ withResponse httpReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

{- Health -}
getServiceChecks :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m [Check]
getServiceChecks manager hostname (PortNum portNumber) name = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/health/checks/", name]
  liftIO $ withResponse initReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ maybe [] id (decode $ BL.fromStrict body)

getServiceHealth :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m Health


{- Catalog -}
getDatacenters :: MonadIO m => Manager -> Text -> PortNumber -> m [Datacenter]
getDatacenters manager hostname (PortNum portNumber) = liftIO $ do
  initReq <- parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/catalog/datacenters/"]
  withResponse initReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let val = (decode $ BL.fromStrict body)
    case val of
      Just x -> return x
      Nothing -> return []

