{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Client.Init
  ( initializeConsulClient
  , initializeTlsConsulClient
  ) where

import Import

-- | TODO: Document
initializeConsulClient :: MonadIO m => Text -> PortNumber -> Maybe Manager -> m ConsulClient
initializeConsulClient hostname port man = do
  manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newTlsManager
  return $ ConsulClient manager hostname port False Nothing
                                                 -- we omit a Datacenter here for brevity
                                                 -- it's still allowed via record updates

-- | TODO: Document
initializeTlsConsulClient :: MonadIO m => Text -> PortNumber -> Maybe Manager -> m ConsulClient
initializeTlsConsulClient hostname port man = do
    manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newTlsManagerWith tlsManagerSettings
    return $ ConsulClient manager hostname port True Nothing

