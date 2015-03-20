{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Consul (
    createManagedSession
  , deleteKey
  , destroyManagedSession
  , getKey
  , initializeConsulClient
  , listKeys
  , putKey
  , putKeyAcquireLock
  , putKeyReleaseLock
  , withManagedSession
  , Consistency(..)
  , ConsulClient(..)
  , Datacenter(..)
  , KeyValue(..)
  , KeyValuePut(..)
  , ManagedSession(..)
  , Session(..)
) where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import qualified Network.Consul.Internal as I
import Data.Word
import Network.Consul.Types
import Network.HTTP.Client (defaultManagerSettings, newManager, ManagerSettings)
import Network.Socket (PortNumber)

import Prelude hiding (mapM)

initializeConsulClient :: MonadIO m => Text -> PortNumber -> Maybe ManagerSettings -> m ConsulClient
initializeConsulClient hostname port settings = do
  manager <- liftIO $ case settings of
                        Just x -> newManager x
                        Nothing -> newManager defaultManagerSettings
  return $ ConsulClient manager hostname port


{- Key Value -}

getKey :: MonadIO m => ConsulClient -> Text -> Maybe Word64 -> Maybe Consistency -> Maybe Datacenter -> m (Maybe KeyValue)
getKey _client@ConsulClient{..} = I.getKey ccManager ccHostname ccPort

getKeys :: MonadIO m => ConsulClient -> Text -> Maybe Word64 -> Maybe Consistency -> Maybe Datacenter -> m [KeyValue]
getKeys _client@ConsulClient{..} = I.getKeys ccManager ccHostname ccPort

listKeys :: MonadIO m => ConsulClient -> Text -> Maybe Word64 -> Maybe Consistency -> Maybe Datacenter -> m [Text]
listKeys _client@ConsulClient{..} = I.listKeys ccManager ccHostname ccPort

putKey :: MonadIO m => ConsulClient -> KeyValuePut -> Maybe Datacenter -> m Bool
putKey _client@ConsulClient{..} = I.putKey ccManager ccHostname ccPort

putKeyAcquireLock :: MonadIO m => ConsulClient -> KeyValuePut -> Session -> Maybe Datacenter -> m Bool
putKeyAcquireLock _client@ConsulClient{..} = I.putKeyAcquireLock ccManager ccHostname ccPort

putKeyReleaseLock :: MonadIO m => ConsulClient -> KeyValuePut -> Session -> Maybe Datacenter -> m Bool
putKeyReleaseLock _client@ConsulClient{..} = I.putKeyReleaseLock ccManager ccHostname ccPort

deleteKey :: MonadIO m => ConsulClient -> Text -> Bool -> Maybe Datacenter -> m ()
deleteKey _client@ConsulClient{..} key = I.deleteKey ccManager ccHostname ccPort key

{- Agent -}

{- Helper Functions -}

{- ManagedSession is a session with an associated TTL healthcheck so the session will be terminated if the client dies. The healthcheck will be automatically updated. -}
data ManagedSession = ManagedSession{
  msSession :: Session,
  msThreadId :: ThreadId
}

withManagedSession :: MonadIO m => ConsulClient -> Int -> (Session -> m ()) -> m ()
withManagedSession client ttl action = do
  x <- createManagedSession client Nothing ttl
  case x of
    Just s -> action (msSession s) >> destroyManagedSession client s
    Nothing -> return ()

createManagedSession :: MonadIO m => ConsulClient -> Maybe Text -> Int -> m (Maybe ManagedSession)
createManagedSession _client@ConsulClient{..} name ttl = do
  let r = SessionRequest Nothing name Nothing [] (Just Release) (Just ttl)
  s <- I.createSession ccManager ccHostname ccPort r Nothing
  mapM f s
  where
    f x = do
      tid <- liftIO $ forkIO $ runThread x
      return $ ManagedSession x tid

    runThread :: Session -> IO ()
    runThread s = do
      threadDelay 10
      I.renewSession ccManager ccHostname ccPort s Nothing
      return ()

destroyManagedSession :: MonadIO m => ConsulClient -> ManagedSession -> m ()
destroyManagedSession _client@ConsulClient{..} (ManagedSession session tid) = do
  liftIO $ killThread tid
  I.destroySession ccManager ccHostname ccPort session Nothing
