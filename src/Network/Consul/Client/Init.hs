{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | __Consul Client Initialization__

The functions in this module are used to initialize the `ConsulClient`.

This documentation is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).
-}
module Network.Consul.Client.Init
  ( initializeConsulClient
  , initializeTlsConsulClient
  ) where

import Import


{- | Initialize non-Tls Client

@since 0.0.0.0
-}
-- | TODO: Document
initializeConsulClient
  :: MonadIO m => Text -- ^
  -> PortNumber -- ^
  -> Maybe Manager -- ^
  -> m ConsulClient -- ^
initializeConsulClient hostname port man = do
  manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newTlsManager
  return $ ConsulClient manager hostname port False Nothing
                                                 -- we omit a Datacenter here for brevity
                                                 -- it's still allowed via record updates


{- | Initialize Tls Client

@since 0.0.0.0
-}
-- | TODO: Document
initializeTlsConsulClient
  :: MonadIO m => Text -- ^
  -> PortNumber -- ^
  -> Maybe Manager -- ^
  -> m ConsulClient -- ^
initializeTlsConsulClient hostname port man = do
    manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newTlsManagerWith tlsManagerSettings
    return $ ConsulClient manager hostname port True Nothing

