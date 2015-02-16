{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Consul (
    deleteKey
  , getKey
  , initializeConsulClient
  , putKey
  , ConsulClient
) where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Network.Consul.Internal as I
import Network.Consul.Types
import Network.HTTP.Client (defaultManagerSettings, newManager, ManagerSettings)
import Network.Socket (PortNumber)

initializeConsulClient :: MonadIO m => Text -> PortNumber -> Maybe ManagerSettings -> m ConsulClient
initializeConsulClient hostname port settings = do
  manager <- liftIO $ case settings of
                        Just x -> newManager x
                        Nothing -> newManager defaultManagerSettings
  return $ ConsulClient manager hostname port

getKey :: MonadIO m => ConsulClient -> KeyValueRequest -> m (Maybe KeyValue)
getKey _client@ConsulClient{..} request = I.getKey ccManager ccHostname ccPort request

putKey :: MonadIO m => ConsulClient -> KeyValuePut -> m Text
putKey _client@ConsulClient{..} request = I.putKey ccManager ccHostname ccPort request

deleteKey :: MonadIO m => ConsulClient -> Text -> m ()
deleteKey _client@ConsulClient{..} key = I.deleteKey ccManager ccHostname ccPort key
