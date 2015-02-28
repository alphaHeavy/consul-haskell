{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Consul (
    createManagedSession
  , deleteKey
  , getKey
  , initializeConsulClient
  , putKey
  , ConsulClient(..)
  , ManagedSession(..)
) where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import qualified Network.Consul.Internal as I
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


getKey :: MonadIO m => ConsulClient -> m (Maybe KeyValue)
getKey _client@ConsulClient{..} = undefined -- I.getKey ccManager ccHostname ccPort request

--listKeys :: MonadIO m => ConsulClient ->

putKey :: MonadIO m => ConsulClient -> KeyValuePut -> m Text
putKey _client@ConsulClient{..} request = undefined -- I.putKey ccManager ccHostname ccPort request

deleteKey :: MonadIO m => ConsulClient -> Text -> m ()
deleteKey _client@ConsulClient{..} key = undefined --I.deleteKey ccManager ccHostname ccPort key


{- Agent -}

{- Helper Functions -}

{- ManagedSession is a session with an associated TTL healthcheck so the session will be terminated if the client dies. The healthcheck will be automatically updated. -}
data ManagedSession = ManagedSession{
  msSession :: Session,
  msThreadId :: ThreadId
}

createManagedSession :: MonadIO m => ConsulClient -> Maybe Text -> Int -> m (Maybe ManagedSession)
createManagedSession client@ConsulClient{..} name ttl = do
  let r = SessionRequest Nothing name Nothing [] (Just Release) (Just ttl)
  s <- I.createSession ccManager ccHostname ccPort r Nothing
  mapM f s
  where
    f x = do
      tid <- liftIO $ forkIO $ runThread client x
      return $ ManagedSession x tid

    runThread :: ConsulClient -> Session -> IO ()
    runThread _client@ConsulClient{..} s = do
      threadDelay 10
      I.renewSession ccManager ccHostname ccPort s Nothing
      return ()
