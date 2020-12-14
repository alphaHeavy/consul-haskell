{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Consul (
    createSession
  , deleteKey
  , destroySession
  , deregisterService
  , getDatacenters
  , getKey
  , getKeys
  , getSelf
  , getService
  , getServiceChecks
  , getServices
  , getServiceHealth
  , getSessionInfo
  , getSequencerForLock
  , initializeConsulClient
  , initializeTlsConsulClient
  , isValidSequencer
  , listKeys
  , passHealthCheck
  , putKey
  , putKeyAcquireLock
  , putKeyReleaseLock
  , registerService
  , renewSession
  , runService
  , withSession
  , withSequencer
  --Agent
  , deregisterHealthCheck
  , failHealthCheck
  , registerHealthCheck
  , warnHealthCheck
  , module Network.Consul.Types
) where

import Control.Concurrent hiding (killThread)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadMask)
import Control.Retry
import Data.Aeson (Value(..), decode,encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word
import qualified Data.Vector as V
import qualified Network.Consul.Internal as I
import Network.Consul.Types
import Network.HTTP.Client -- (method, Manager, responseBody)
import Network.HTTP.Client.TLS (newTlsManager, newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types
import Network.Socket (PortNumber)
import UnliftIO (MonadUnliftIO, async, cancel, finally, wait, waitAnyCancel, withAsync)

import Network.Consul.Internal

import Prelude hiding (mapM)

parseTtl :: Integral t => Text -> t
parseTtl ttl = let Right (x,_) = TR.decimal $ T.filter (/= 's') ttl in x

initializeConsulClient :: MonadIO m => Text -> PortNumber -> Maybe Manager -> m ConsulClient
initializeConsulClient hostname port man = do
  manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newTlsManager
  return $ ConsulClient manager hostname port False Nothing
                                                 -- ^ we omit a Datacenter here for brevity
                                                 --   it's still allowed via record updates 


initializeTlsConsulClient :: MonadIO m => Text -> PortNumber -> Maybe Manager -> m ConsulClient
initializeTlsConsulClient hostname port man = do
    manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newTlsManagerWith tlsManagerSettings
    return $ ConsulClient manager hostname port True Nothing


{- Key Value -}
getKey :: MonadIO m => ConsulClient -> Text -> Maybe Word64 -> Maybe Consistency -> m (Maybe KeyValue)
getKey _client@ConsulClient{..} key index consistency = do
  let hostnameWithScheme = hostWithScheme _client
  request <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/",key])
                           fquery
                           Nothing
                           (isJust index)
                           ccDatacenter
  liftIO $ withResponse request ccManager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> do
        bodyParts <- brConsume $ responseBody response
        let body = B.concat bodyParts
        return $ listToMaybe =<< (decode $ BL.fromStrict body)
      _ -> return Nothing
  where
    cons = fmap (\ x -> T.concat["consistency=", T.pack $ show x] ) consistency
    ind = fmap (\ x -> T.concat["index=", T.pack $ show x]) index
    query = T.intercalate "&" $ catMaybes [cons,ind]
    fquery = if query /= T.empty then Just query else Nothing


getKeys :: MonadIO m => ConsulClient -> Text -> Maybe Word64 -> Maybe Consistency -> m [KeyValue]
getKeys _client@ConsulClient{..} key index consistency = do
  let hostnameWithScheme = hostWithScheme _client
  request <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/",key])
                           fquery
                           Nothing
                           (isJust index)
                           ccDatacenter
  liftIO $ withResponse request ccManager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> do
        bodyParts <- brConsume $ responseBody response
        let body = B.concat bodyParts
        return $ maybe [] id $ decode $ BL.fromStrict body
      _ -> return []
  where
    cons = fmap (\ x -> T.concat["consistency=", T.pack $ show x] ) consistency
    ind = fmap (\ x -> T.concat["index=", T.pack $ show x]) index
    query = T.intercalate "&" $ catMaybes [cons,ind, Just "recurse"]
    fquery = if query /= T.empty then Just query else Nothing


listKeys :: MonadIO m => ConsulClient -> Text -> Maybe Word64 -> Maybe Consistency -> m [Text]
listKeys _client@ConsulClient{..} prefix index consistency = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", prefix])
                           fquery
                           Nothing
                           (isJust index)
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response ->
    case responseStatus response of
      x | x == status200 -> do
        bodyParts <- brConsume $ responseBody response
        let body = B.concat bodyParts
        return $ maybe [] id $ decode $ BL.fromStrict body
      _ -> return []
  where
    cons = fmap (\ x -> T.concat["consistency=", T.pack $ show x] ) consistency
    ind = fmap (\ x -> T.concat["index=", T.pack $ show x]) index
    query = T.intercalate "&" $ catMaybes [cons,ind, Just "keys"]
    fquery = if query /= T.empty then Just query else Nothing


putKey :: MonadIO m => ConsulClient -> KeyValuePut -> m Bool
putKey _client@ConsulClient{..} putRequest = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", kvpKey putRequest])
                           fquery
                           (Just $ kvpValue putRequest)
                           False
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let result = I.decodeAndStrip body
    case result of
      "true" -> return True
      "false" -> return False
      _ -> return False
  where
    flags = fmap (\ x -> T.concat["flags=", T.pack $ show x]) $ kvpFlags putRequest
    cas = fmap (\ x -> T.concat["cas=",T.pack $ show x]) $ kvpCasIndex putRequest
    query = T.intercalate "&" $ catMaybes [flags,cas]
    fquery = if query /= T.empty then Just query else Nothing



putKeyAcquireLock :: MonadIO m => ConsulClient -> KeyValuePut -> Session -> m Bool
putKeyAcquireLock _client@ConsulClient{..} request (Session session _) = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", kvpKey request])
                           fquery
                           (Just $ kvpValue request)
                           False
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let result = decodeAndStrip body
    case result of
      "true" -> return True
      "false" -> return False
      _ -> return False
  where
    flags = fmap (\ x -> T.concat["flags=", T.pack $ show x]) $ kvpFlags request
    cas = fmap (\ x -> T.concat["cas=",T.pack $ show x]) $ kvpCasIndex request
    acquire = T.concat["acquire=",session]
    query = T.intercalate "&" $ catMaybes [flags,cas,Just acquire]
    fquery = if query /= T.empty then Just query else Nothing


putKeyReleaseLock :: MonadIO m => ConsulClient -> KeyValuePut -> Session -> m Bool
putKeyReleaseLock _client@ConsulClient{..} request (Session session _) = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", kvpKey request])
                           fquery
                           (Just $ kvpValue request)
                           False
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let result = decodeAndStrip body
    case result of
      "true" -> return True
      "false" -> return False
      _ -> return False
  where
    flags = fmap (\ x -> T.concat["flags=", T.pack $ show x]) $ kvpFlags request
    cas = fmap (\ x -> T.concat["cas=",T.pack $ show x]) $ kvpCasIndex request
    release = T.concat["release=",session]
    query = T.intercalate "&" $ catMaybes [flags,cas,Just release]
    fquery = if query /= T.empty then Just query else Nothing


deleteKey :: MonadIO m => ConsulClient -> Text -> Bool -> m Bool
deleteKey _client@ConsulClient{..} key recurse = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", key])
                           (if recurse then Just "recurse" else Nothing)
                           Nothing
                           False
                           ccDatacenter
  let httpReq = initReq { method = "DELETE"}
  liftIO $ withResponse httpReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let result = decodeAndStrip body
    case result of
      "true" -> return True
      "false" -> return False
      _ -> return False


{- Health Checks -}
getServiceChecks :: MonadIO m => ConsulClient -> Text -> m [Check]
getServiceChecks _client@ConsulClient{..} name = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/health/checks", name])
                           Nothing
                           Nothing
                           False
                           Nothing
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ maybe [] id (decode $ BL.fromStrict body)


getServiceHealth :: MonadIO m => ConsulClient -> Text -> m (Maybe [Health])
getServiceHealth _client@ConsulClient{..} name = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/health/service/", name]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ decode $ BL.fromStrict body


{- Catalog -}
getDatacenters :: MonadIO m => ConsulClient -> m [Datacenter]
getDatacenters client@ConsulClient{..} = liftIO $ do
  let hostnameWithScheme = hostWithScheme client
  initReq <- parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/catalog/datacenters/"]
  withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let val = (decode $ BL.fromStrict body)
    case val of
      Just x -> return x
      Nothing -> return []


getService :: MonadIO m => ConsulClient -> Text -> Maybe Text -> m (Maybe [ServiceResult])
getService _client@ConsulClient{..} name tag = do
  let hostnameWithScheme = hostWithScheme _client
  req <- createRequest hostnameWithScheme
                       ccPort
                       (T.concat["/v1/catalog/service/",name])
                       (fmap (\ x -> T.concat ["tag=",x]) tag)
                       Nothing
                       False
                       ccDatacenter

  liftIO $ withResponse req ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    return $ decode $ BL.fromStrict $ B.concat bodyParts


getServices :: MonadIO m => ConsulClient -> Maybe Text -> m [Text]
getServices _client@ConsulClient{..} tag = do
    req <- createRequest (hostWithScheme _client)
                         ccPort
                         "/v1/catalog/services"
                         Nothing
                         Nothing
                         False
                         ccDatacenter
    liftIO $ withResponse req ccManager $ \ response -> do
        bodyParts <- brConsume $ responseBody response
        return $ parseServices tag $ decode $ BL.fromStrict $ B.concat bodyParts
  where
    parseServices t (Just (Object v)) = filterTags t $ H.toList v
    parseServices _   _               = []
    filterTags :: Maybe Text -> [(Text, Value)] -> [Text]
    filterTags (Just t)               = map fst . filter (\ (_, (Array v)) -> (String t) `V.elem` v)
    filterTags Nothing                = map fst


{- Agent -}
getSelf :: MonadIO m => ConsulClient -> m (Maybe Self)
getSelf _client@ConsulClient{..} =  do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/agent/self"]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ decode $ BL.fromStrict body


runService :: MonadUnliftIO m => ConsulClient -> RegisterService -> m () -> m ()
runService client request action = do
  r <- registerService client request
  case r of
    True -> do
      mainFunc <- async action

      --this is here instead of the where to prevent typechecking nastiness
      checkAction <- case rsCheck request of
                      Just(x@(Ttl _)) -> do
                        a <- async $ forever $ ttlFunc x
                        return $ Just a
                      _ -> return Nothing

      _foo :: () <- wait mainFunc --prevent: 'StM’ is a type function, and may not be injective
      case checkAction of
        Just a -> cancel a
        Nothing -> return ()
    False -> return ()
  where
    ttlFunc y@(Ttl x) = do
      let ttl = parseTtl x
      liftIO $ threadDelay $ (ttl - (fromIntegral $ floor (fromIntegral ttl / fromIntegral 2))) * 1000000
      let checkId = T.concat["service:",maybe (rsName request) id (rsId request)]
      passHealthCheck client checkId


{- Session -}
createSession :: MonadIO m => ConsulClient -> SessionRequest -> m (Maybe Session)
createSession client@ConsulClient{..} request = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           "/v1/session/create"
                           noQuery
                           (Just $ BL.toStrict $ encode request)
                           waitFalse
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> do
        bodyParts <- brConsume $ responseBody response
        return $ decode $ BL.fromStrict $ B.concat bodyParts
      _ -> return Nothing


destroySession :: MonadIO m => ConsulClient -> Session ->  m ()
destroySession client@ConsulClient{..} (Session session _) = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/session/destroy/", session])
                           Nothing
                           Nothing
                           False
                           ccDatacenter
  let req = initReq{method = "PUT"}
  liftIO $ withResponse req ccManager $ \ _response -> return ()


renewSession :: MonadIO m => ConsulClient -> Session ->  m Bool
renewSession client@ConsulClient{..} (Session session _) =  do
  let hostnameWithScheme = hostWithScheme client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/session/renew/", session])
                           Nothing
                           Nothing
                           False
                           ccDatacenter
  let req = initReq{method = "PUT"}
  liftIO $ withResponse req ccManager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> return True
      _ -> return False


getSessionInfo :: MonadIO m => ConsulClient -> Session ->  m (Maybe [SessionInfo])
getSessionInfo client@ConsulClient{..} (Session sessionId _) = do
  let hostnameWithScheme = hostWithScheme client
  req <- createRequest hostnameWithScheme
                       ccPort
                       (T.concat ["/v1/session/info/",sessionId])
                       noQuery
                       noRequestBody
                       waitFalse
                       ccDatacenter
  liftIO $ withResponse req ccManager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> do
        bodyParts <- brConsume $ responseBody response
        return $ decode $ BL.fromStrict $ B.concat bodyParts
      _ -> return Nothing


-- TODO: use `name` in function?
withSession :: forall m a. (MonadMask m, MonadUnliftIO m) => ConsulClient -> Maybe Text -> Int -> Session -> (Session -> m a) -> m a -> m a
withSession client@ConsulClient{..} name delay session action lostAction = (do
  withAsync (action session) $ \ mainAsync -> withAsync extendSession $ \ extendAsync -> do
    result :: a <- return . snd =<< waitAnyCancel [mainAsync,extendAsync]
    return result) `finally` (destroySession client session)
  where
    extendSession :: m a
    extendSession = do
      liftIO $ threadDelay $ (delay * 1000000)
      x <- renewSession client session
      case x of
        True -> extendSession
        False -> lostAction


getSequencerForLock :: MonadIO m => ConsulClient -> Text -> Session -> m (Maybe Sequencer)
getSequencerForLock client key session = do
  let dc = ccDatacenter client
  kv <- getKey client key Nothing (Just Consistent)
  case kv of
    Just k -> do
      let isValid = maybe False ((sId session) ==) $ kvSession k
      if isValid then return $ Just $ Sequencer key (kvLockIndex k) session else return Nothing
    Nothing -> return Nothing


isValidSequencer :: MonadIO m => ConsulClient -> Sequencer -> m Bool
isValidSequencer client sequencer = do
  mkv <- getKey client (sKey sequencer) Nothing (Just Consistent)
  case mkv of
    Just kv -> return $ (maybe False ((sId $ sSession sequencer) ==) $ kvSession kv) && (kvLockIndex kv) == (sLockIndex sequencer)
    Nothing -> return False


withSequencer :: (MonadMask m, MonadUnliftIO m) => ConsulClient -> Sequencer -> m a -> m a -> Int -> m a
withSequencer client sequencer action lostAction delay =
  withAsync action $ \ mainAsync -> withAsync pulseLock $ \ pulseAsync -> do
    waitAnyCancel [mainAsync, pulseAsync] >>= return . snd
  where
    pulseLock = recoverAll (exponentialBackoff 50000 <>  limitRetries 5) $ \ _ -> do
      liftIO $ threadDelay delay
      valid <- isValidSequencer client sequencer
      case valid of
        True -> pulseLock
        False -> lostAction


{- Agent -}
{-getHealthChecks :: MonadIO m => Manager -> Text -> PortNumber -> Maybe Datacenter -> m [Check]
getHealthChecks  manager hostname portNumber dc = do
  request <- createRequest hostname portNumber "/agent/checks" Nothing Nothing False dc
 -}

registerHealthCheck :: MonadIO m => ConsulClient -> RegisterHealthCheck -> m ()
registerHealthCheck client@ConsulClient{..} request = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/agent/check/register"]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ BL.toStrict $ encode request}
  liftIO $ withResponse httpReq ccManager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

deregisterHealthCheck :: MonadIO m => ConsulClient -> Text -> m ()
deregisterHealthCheck client@ConsulClient{..} checkId = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/agent/check/deregister/", checkId])
                           Nothing
                           Nothing
                           False
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()


passHealthCheck :: MonadIO m => ConsulClient -> Text -> m ()
passHealthCheck client checkId = do
  -- Using `Just ""` as the `body` to ensure a PUT request is used.
  -- Consul < 1.0 accepted a GET here (which was a legacy mistake).
  -- In 1.0, they switched it to require a PUT.
  -- See also:
  --   * https://github.com/hashicorp/consul/issues/3659
  --   * https://github.com/cablehead/python-consul/pull/182
  --   * https://github.com/hashicorp/consul/blob/51ea240df8476e02215d53fbfad5838bf0d44d21/CHANGELOG.md
  --     Section "HTTP Verbs are Enforced in Many HTTP APIs":
  --     > Many endpoints in the HTTP API that previously took any HTTP verb
  --     > now check for specific HTTP verbs and enforce them.
  let portNumber = ccPort client
      manager = ccManager client
      hostname = hostWithScheme client
      dc = ccDatacenter client
      --hostname = checkId
  initReq <- createRequest hostname
                           portNumber
                           (T.concat ["/v1/agent/check/pass/", checkId])
                           Nothing
                           (Just "")
                           False
                           dc
  liftIO $ withResponse initReq manager $ \ _response -> do
    return ()


warnHealthCheck :: MonadIO m => ConsulClient -> Text -> m ()
warnHealthCheck client@ConsulClient{..} checkId = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/agent/check/warn/", checkId]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()


failHealthCheck :: MonadIO m => ConsulClient -> Text -> m ()
failHealthCheck client@ConsulClient{..} checkId = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/agent/check/fail/", checkId]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()



registerService :: MonadIO m => ConsulClient -> RegisterService -> m Bool
registerService client request = do
  let portNumber = ccPort client
      manager = ccManager client
      hostname = hostWithScheme client
      dc = ccDatacenter client
  initReq <- createRequest hostname
                           portNumber
                           "/v1/agent/service/register"
                           Nothing
                           (Just $ BL.toStrict $ encode request)
                           False
                           dc
  liftIO $ withResponse initReq manager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> return True
      _ -> return False


deregisterService :: MonadIO m => ConsulClient -> Text -> m ()
deregisterService client service = do
  let portNumber = ccPort client
      manager = ccManager client
      hostname = hostWithScheme client
      dc = ccDatacenter client
  initReq <- createRequest hostname
                           portNumber
                           (T.concat ["/v1/agent/service/deregister/", service])
                           Nothing
                           (Just "") -- forces PUT
                           False
                           dc
  liftIO $ withResponse initReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()
