{-# LANGUAGE OverloadedStrings #-}

module Network.Consul.Internal (
  --Key-Value Store
    deleteKey
  , getKey
  , listKeys
  , putKey
  , putKeyAcquireLock
  , putKeyReleaseLock

  --Agent
  , deregisterHealthCheck
  , deregisterService
  , failHealthCheck
  , passHealthCheck
  , registerHealthCheck
  , registerService
  , warnHealthCheck

  --Health
  , getServiceChecks
  , getServiceHealth

  -- Session
  , createSession

  --Catalog
  , getDatacenters
  ) where

import Control.Monad.IO.Class
import Data.Aeson (decode,encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Consul.Types
import Network.HTTP.Client
import Network.Socket (PortNumber(..))

createRequest :: MonadIO m => Text -> PortNumber -> Text -> Maybe Text-> Maybe ByteString -> Maybe Datacenter -> m Request
createRequest hostname (PortNum portNumber) endpoint queryString body dc = do
  let baseUrl = T.concat ["http://",hostname,":",T.pack $ show portNumber,endpoint,needQueryString,maybe "" id queryString, maybe "" (\ (Datacenter x) -> T.concat["&",x]) dc]
  initReq <- liftIO $ parseUrl $ T.unpack baseUrl
  case body of
    Just x -> return initReq{ method = "PUT", requestBody = RequestBodyBS x, checkStatus = \ _ _ _ -> Nothing}
    Nothing -> return initReq{checkStatus = \ _ _ _ -> Nothing}

  where
    needQueryString = if isJust dc || isJust queryString then "?" else ""

{- Key Value Store -}
getKey :: MonadIO m => Manager -> Text -> PortNumber -> Text -> Maybe Consistency -> Maybe Datacenter -> m (Maybe KeyValue)
getKey manager hostname port key consistency dc = do
  request <- createRequest hostname port (T.concat ["/v1/kv/",key]) (fmap (\ x -> T.concat["consistency=", T.pack $ show x]) consistency) Nothing dc
  liftIO $ do
   withResponse request manager $ \ response -> do
    case responseStatus response of
      status200 -> do
        bodyParts <- brConsume $ responseBody response
        let body = B.concat bodyParts
        return $ listToMaybe =<< (decode $ BL.fromStrict body)
      _ -> return Nothing

listKeys :: MonadIO m => Manager -> Text -> PortNumber -> Text -> Maybe Consistency -> Maybe Datacenter -> m [Text]
listKeys manager hostname (PortNum portNumber) prefix consistency dc = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", prefix,"?keys"]
  liftIO $ withResponse initReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ maybe [] id $ decode $ BL.fromStrict body


putKey :: MonadIO m => Manager -> Text -> PortNumber -> KeyValuePut -> m Text
putKey manager hostname (PortNum portNumber) request = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", kvpKey request]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ kvpValue request}
  liftIO $ withResponse httpReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ TE.decodeUtf8 body

putKeyAcquireLock :: MonadIO m => Manager -> Text -> PortNumber -> KeyValuePut -> Session -> m Bool
putKeyAcquireLock manager hostname (PortNum portNumber) request session = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", kvpKey request,"?acquire=",sId session]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ kvpValue request}
  liftIO $ withResponse httpReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    case TE.decodeUtf8 body of
      "true" -> return True
      "false" -> return False

putKeyReleaseLock :: MonadIO m => Manager -> Text -> PortNumber -> KeyValuePut -> Session -> m Bool
putKeyReleaseLock manager hostname (PortNum portNumber) request session = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", kvpKey request,"?release=",sId session]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ kvpValue request}
  liftIO $ withResponse httpReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    case TE.decodeUtf8 body of
      "true" -> return True
      "false" -> return False

deleteKey :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m ()
deleteKey manager hostname (PortNum portNumber) key = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/kv/", key]
  let httpReq = initReq { method = "DELETE"}
  liftIO $ withResponse httpReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

{- Agent -}
registerHealthCheck :: MonadIO m => Manager -> Text -> PortNumber -> RegisterHealthCheck -> m ()
registerHealthCheck manager hostname (PortNum portNumber) request = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/agent/check/register"]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ BL.toStrict $ encode request}
  liftIO $ withResponse httpReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

deregisterHealthCheck :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m ()
deregisterHealthCheck manager hostname (PortNum portNumber) checkId = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/agent/check/deregister/", checkId]
  liftIO $ withResponse initReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()


passHealthCheck :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m ()
passHealthCheck manager hostname (PortNum portNumber) checkId = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/agent/check/pass/", checkId]
  liftIO $ withResponse initReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

warnHealthCheck :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m ()
warnHealthCheck manager hostname (PortNum portNumber) checkId = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/agent/check/warn/", checkId]
  liftIO $ withResponse initReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

failHealthCheck :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m ()
failHealthCheck manager hostname (PortNum portNumber) checkId = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/agent/check/fail/", checkId]
  liftIO $ withResponse initReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

registerService :: MonadIO m => Manager -> Text -> PortNumber -> RegisterService -> m ()
registerService manager hostname (PortNum portNumber) request = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/agent/service/register"]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ BL.toStrict $ encode request}
  liftIO $ withResponse httpReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

deregisterService :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m ()
deregisterService manager hostname (PortNum portNumber) service = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/agent/service/deregister/", service]
  liftIO $ withResponse initReq manager $ \ response -> do
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

getServiceHealth :: MonadIO m => Manager -> Text -> PortNumber -> Text -> m (Maybe Health)
getServiceHealth manager hostname (PortNum portNumber) name = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/health/service/", name]
  liftIO $ withResponse initReq manager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ decode $ BL.fromStrict body

{- Session -}
createSession :: MonadIO m => Manager -> Text -> PortNumber -> Maybe Datacenter -> SessionRequest -> m (Maybe Session)
createSession manager hostname (PortNum portNumber) datacenter request = do
  initReq <- liftIO $ parseUrl $ T.unpack $ T.concat ["http://",hostname, ":", T.pack $ show portNumber ,"/v1/session/create"]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ BL.toStrict $ encode request}
  liftIO $ withResponse httpReq manager $ \ response -> do
    case responseStatus response of
      status200 -> do
        bodyParts <- brConsume $ responseBody response
        return $ decode $ BL.fromStrict $ B.concat bodyParts
      _ -> return Nothing

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

