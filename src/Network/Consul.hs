{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Consul (
    createManagedSession
  , deleteKey
  , deregisterService
  , destroyManagedSession
  , getKey
  , getKeys
  , getSelf
  , getService
  , getServices
  , getServiceHealth
  , getSessionInfo
  , getSequencerForLock
  , initializeConsulClient
  , initializeTlsConsulClient
  , isValidSequencer
  , listKeys
  , ManagedSession (..)
  , passHealthCheck
  , putKey
  , putKeyAcquireLock
  , putKeyReleaseLock
  , registerService
  , runService
  , withManagedSession
--  , withSequencer
  , withSession
  , module Network.Consul.Types
) where

import Control.Concurrent hiding (killThread)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted (fork, killThread)
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

getServices :: MonadIO m => ConsulClient -> Maybe Text -> Maybe Datacenter -> m [Text]
getServices _client@ConsulClient{..} = I.getServices ccManager (I.hostWithScheme _client) ccPort

{- Agent -}
getSelf :: MonadIO m => ConsulClient -> m (Maybe Self)
getSelf _client@ConsulClient{..} = I.getSelf ccManager (I.hostWithScheme _client) ccPort

deregisterService :: MonadIO m => ConsulClient -> Text -> m ()
deregisterService _client@ConsulClient{..} = I.deregisterService ccManager (I.hostWithScheme _client) ccPort 

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
      _foo :: () <- wait mainFunc
      case checkAction of
        Just a -> cancel a
        Nothing -> return ()
    False -> return ()
  where
    ttlFunc y@(Ttl x) = do
      (do
        let ttl = parseTtl x
        liftIO $ threadDelay $ (ttl - (fromIntegral $ floor (fromIntegral ttl / fromIntegral 2))) * 1000000
        let checkId = T.concat["service:",maybe (rsName request) id (rsId request)]
        passHealthCheck client checkId dc) `catch` (\ e -> do
           let _x :: SomeException = e
           return ())
      ttlFunc y

{- Session -}
getSessionInfo :: MonadIO m => ConsulClient -> Text -> Maybe Datacenter -> m (Maybe [SessionInfo])
getSessionInfo _client@ConsulClient{..} = I.getSessionInfo ccManager (I.hostWithScheme _client) ccPort

withSession :: forall a m. (MonadIO m,MonadBaseControl IO m) => ConsulClient -> Session -> (Session -> m a) -> m a -> m a
withSession client session action lostAction = do
  var <- liftIO $ newEmptyTMVarIO
  tidVar <- liftIO $ newEmptyTMVarIO
  stid <- fork $ runThread var tidVar
  tid <- fork $ action session >>= \ x -> liftIO $ atomically $ putTMVar var x
  liftIO $ atomically $ putTMVar tidVar tid
  ret <- liftIO $ atomically $ takeTMVar var
  killThread stid
  return ret
  where
    runThread :: TMVar a -> TMVar ThreadId -> m ()
    runThread var threadVar = do
      liftIO $ threadDelay (10 * 1000000)
      x <- getSessionInfo client (sId session) Nothing
      case x of
        Just [] -> cancelAction var threadVar
        Nothing -> cancelAction var threadVar
        Just _ -> runThread var threadVar

    cancelAction :: TMVar a -> TMVar ThreadId -> m ()
    cancelAction resultVar tidVar = do
      tid <- liftIO $ atomically $ readTMVar tidVar
      killThread tid
      empty <- liftIO $ atomically $ isEmptyTMVar resultVar
      if empty then do
        result <- lostAction
        liftIO $ atomically $ putTMVar resultVar result
        return ()
        else return ()

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
{- 
withSequencer :: (MonadBaseControl IO m, MonadIO m, MonadMask m) => ConsulClient -> Sequencer -> m a -> m a -> Int -> Maybe Datacenter -> m a
withSequencer client sequencer action lostAction delay dc = do
  mainFunc <- async action
  pulseFunc <- async pulseLock
  waitAny [mainFunc, pulseFunc] >>= return . snd
  where
    pulseLock = recoverAll (exponentialBackoff 50000 <> limitRetries 5) $ do
      liftIO $ threadDelay delay
      valid <- isValidSequencer client sequencer dc
      case valid of
        True -> pulseLock
        False -> lostAction
-}

{- Helper Functions -}
{- ManagedSession is a session with an associated TTL healthcheck so the session will be terminated if the client dies. The healthcheck will be automatically updated. -}
data ManagedSession = ManagedSession{
  msSession :: Session,
  msThreadId :: ThreadId
}

withManagedSession :: (MonadBaseControl IO m, MonadIO m) => ConsulClient -> Text -> (Session -> m ()) -> m () -> m ()
withManagedSession client ttl action lostAction = do
  x <- createManagedSession client Nothing ttl
  case x of
    Just s -> withSession client (msSession s) action lostAction >> destroyManagedSession client s
    Nothing -> lostAction

createManagedSession :: MonadIO m => ConsulClient -> Maybe Text -> Text -> m (Maybe ManagedSession)
createManagedSession _client@ConsulClient{..} name ttl = do
  let r = SessionRequest Nothing name Nothing [] (Just Release) (Just ttl)
  s <- I.createSession ccManager (I.hostWithScheme _client) ccPort r Nothing
  mapM f s
  where
    f x = do
      tid <- liftIO $ forkIO $ runThread x
      return $ ManagedSession x tid

    saneTtl = let Right (x,_) = TR.decimal $ T.filter (/= 's') ttl in x

    runThread :: Session -> IO ()
    runThread s = do
      threadDelay $ (saneTtl - (saneTtl - 10)) * 1000000
      x <- I.renewSession ccManager (I.hostWithScheme _client) ccPort s Nothing
      case x of
        True -> runThread s
        False -> return ()

destroyManagedSession :: MonadIO m => ConsulClient -> ManagedSession -> m ()
destroyManagedSession _client@ConsulClient{..} (ManagedSession session tid) = do
  liftIO $ killThread tid
  I.destroySession ccManager (I.hostWithScheme _client) ccPort session Nothing
