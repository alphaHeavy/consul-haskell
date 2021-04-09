{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
-- https://www.consul.io/api-docs/config
module Network.Consul.Client.Config
  ( putConfig
  , getConfig
  ) where

import Import

-- | Apply (Put) Configuration
putConfig :: MonadIO m => ConsulClient -> m ()
putConfig = undefined

-- | Get Configuration
getConfig :: MonadIO m => ConsulClient -> m ()
getConfig = undefined
