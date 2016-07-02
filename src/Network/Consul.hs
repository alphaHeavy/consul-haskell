{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Consul (
    deleteKey
  , getKey
  , getKeys
  , getSelf
  , getService
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
  , runService
  , withSequencer
  , withSession
  , module Network.Consul.Types
) where

import Control.Concurrent hiding (killThread)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Exception.Lifted
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Control
import Control.Retry
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Traversable
import Data.Word
import qualified Network.Consul.Internal as I
import Network.Consul.Types
import Network.HTTP.Client (defaultManagerSettings, newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Socket (PortNumber)


import Prelude hiding (mapM)

parseTtl :: forall t. Integral t => Text -> t
parseTtl ttl = let Right (x,_) = TR.decimal $ T.filter (/= 's') ttl in x

initializeConsulClient :: MonadIO m => Text -> PortNumber -> Maybe Manager -> m ConsulClient
initializeConsulClient hostname port man = do
  manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newManager defaultManagerSettings
  return $ ConsulClient manager hostname port False

initializeTlsConsulClient :: MonadIO m => Text -> PortNumber -> Maybe Manager -> m ConsulClient
initializeTlsConsulClient hostname port man = do
    manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newManager tlsManagerSettings
    return $ ConsulClient manager hostname port True

{- Key Value -}
getKey :: MonadIO m => ConsulClient -> Text -> Maybe Word64 -> Maybe Consistency -> Maybe Datacenter -> m (Maybe KeyValue)
getKey _client@ConsulClient{..} = I.getKey ccManager (I.hostWithScheme _client) ccPort

getKeys :: MonadIO m => ConsulClient -> Text -> Maybe Word64 -> Maybe Consistency -> Maybe Datacenter -> m [KeyValue]
getKeys _client@ConsulClient{..} = I.getKeys ccManager (I.hostWithScheme _client) ccPort

listKeys :: MonadIO m => ConsulClient -> Text -> Maybe Word64 -> Maybe Consistency -> Maybe Datacenter -> m [Text]
listKeys _client@ConsulClient{..} = I.listKeys ccManager (I.hostWithScheme _client) ccPort

putKey :: MonadIO m => ConsulClient -> KeyValuePut -> Maybe Datacenter -> m Bool
putKey _client@ConsulClient{..} = I.putKey ccManager (I.hostWithScheme _client) ccPort

putKeyAcquireLock :: MonadIO m => ConsulClient -> KeyValuePut -> Session -> Maybe Datacenter -> m Bool
putKeyAcquireLock _client@ConsulClient{..} = I.putKeyAcquireLock ccManager (I.hostWithScheme _client) ccPort

putKeyReleaseLock :: MonadIO m => ConsulClient -> KeyValuePut -> Session -> Maybe Datacenter -> m Bool
putKeyReleaseLock _client@ConsulClient{..} = I.putKeyReleaseLock ccManager (I.hostWithScheme _client) ccPort

deleteKey :: MonadIO m => ConsulClient -> Text -> Bool -> Maybe Datacenter -> m ()
deleteKey _client@ConsulClient{..} key = I.deleteKey ccManager (I.hostWithScheme _client) ccPort key

{- Health Checks -}
passHealthCheck :: MonadIO m => ConsulClient -> Text -> Maybe Datacenter -> m ()
passHealthCheck _client@ConsulClient{..} = I.passHealthCheck ccManager (I.hostWithScheme _client) ccPort

getServiceHealth :: MonadIO m => ConsulClient -> Text -> m (Maybe [Health])
getServiceHealth _client@ConsulClient{..} = I.getServiceHealth ccManager (I.hostWithScheme _client) ccPort

{- Catalog -}
getService :: MonadIO m => ConsulClient -> Text -> Maybe Text -> Maybe Datacenter -> m (Maybe [ServiceResult])
getService _client@ConsulClient{..} = I.getService ccManager (I.hostWithScheme _client) ccPort

{- Agent -}
getSelf :: MonadIO m => ConsulClient -> m (Maybe Self)
getSelf _client@ConsulClient{..} = I.getSelf ccManager (I.hostWithScheme _client) ccPort

registerService :: MonadIO m => ConsulClient -> RegisterService -> Maybe Datacenter -> m Bool
registerService _client@ConsulClient{..} = I.registerService ccManager (I.hostWithScheme _client) ccPort

runService :: (MonadBaseControl IO m, MonadIO m) => ConsulClient -> RegisterService -> m () -> Maybe Datacenter -> m ()
runService client request action dc = do
  r <- registerService client request dc
  case r of
    True -> do
      mainFunc <- async action
      checkAction <- case rsCheck request of
                      Just(x@(Ttl _)) -> do
                        a <- async $ ttlFunc x
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
      passHealthCheck client checkId dc

{- Session -}
getSessionInfo :: MonadIO m => ConsulClient -> Text -> Maybe Datacenter -> m (Maybe [SessionInfo])
getSessionInfo _client@ConsulClient{..} = I.getSessionInfo ccManager (I.hostWithScheme _client) ccPort

withSession :: forall m a. (MonadBaseControl IO m, MonadIO m, MonadMask m) => ConsulClient -> Maybe Text -> Int -> Session -> (Session -> m a) -> m a -> m a
withSession client@ConsulClient{..} name delay session action lostAction = do
  mainFunc :: Async (StM m a) <- async $ action session
  extendFunc :: Async (StM m a) <- async $ extendSession session
  result :: a <- return . snd =<< waitAnyCancel [mainFunc,extendFunc]
  I.destroySession ccManager (I.hostWithScheme client) ccPort session Nothing
  return result
  where
    extendSession :: Session -> m a
    extendSession session = recoverAll (exponentialBackoff 50000 <>  limitRetries 5) $ \ _ -> do
      liftIO $ threadDelay $ (delay * 1000000)
      x <- I.renewSession ccManager (I.hostWithScheme client) ccPort session Nothing
      case x of
        True -> extendSession session
        False -> lostAction

getSequencerForLock :: MonadIO m => ConsulClient -> Text -> Session -> Maybe Datacenter -> m (Maybe Sequencer)
getSequencerForLock client key session datacenter = do
  kv <- getKey client key Nothing (Just Consistent) datacenter
  case kv of
    Just k -> do
      let isValid = maybe False ((sId session) ==) $ kvSession k
      if isValid then return $ Just $ Sequencer key (kvLockIndex k) session else return Nothing
    Nothing -> return Nothing

isValidSequencer :: MonadIO m => ConsulClient -> Sequencer -> Maybe Datacenter -> m Bool
isValidSequencer client sequencer datacenter = do
  mkv <- getKey client (sKey sequencer) Nothing (Just Consistent) datacenter
  case mkv of
    Just kv -> return $ (maybe False ((sId $ sSession sequencer) ==) $ kvSession kv) && (kvLockIndex kv) == (sLockIndex sequencer)
    Nothing -> return False

withSequencer :: (MonadBaseControl IO m, MonadIO m, MonadMask m) => ConsulClient -> Sequencer -> m a -> m a -> Int -> Maybe Datacenter -> m a
withSequencer client sequencer action lostAction delay dc = do
  mainFunc <- async action
  pulseFunc <- async pulseLock
  waitAnyCancel [mainFunc, pulseFunc] >>= return . snd
  where
    pulseLock = recoverAll (exponentialBackoff 50000 <>  limitRetries 5) $ \ _ -> do
      liftIO $ threadDelay delay
      valid <- isValidSequencer client sequencer dc
      case valid of
        True -> pulseLock
        False -> lostAction
