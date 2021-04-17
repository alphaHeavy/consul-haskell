{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | __Config Client API__

The functions in this module correspond to
the [Consul Config API](https://www.consul.io/api/config).

This module is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

__Missing Functions__

* `deleteConfig`
* `getConfig`
* `listConfigs`
* `putConfig`
-}
module Network.Consul.Client.Config
  ( putConfig
  , getConfig
  , listConfigs
  , deleteConfig
  ) where

import Import

{- | Delete Configuration

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
deleteConfig :: MonadIO m => ConsulClient -> m ()
deleteConfig = undefined


{- | Get Configuration

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
getConfig :: MonadIO m => ConsulClient -> m ()
getConfig = undefined


{- | List Configurations

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
listConfigs :: MonadIO m => ConsulClient -> m ()
listConfigs = undefined


{- | Apply (Put) Configuration

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
putConfig :: MonadIO m => ConsulClient -> m ()
putConfig = undefined
