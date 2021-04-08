{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
-- https://www.consul.io/api-docs/snapshot
module Network.Consul.Client.Snapshots
  ( createSnapshot -- generateSnapshot
  , restoreSnapshot
  ) where

import Import

-- | TODO: Document
createSnapshot :: MonadIO m => ConsulClient -> m ()
createSnapshot = undefined

-- | TODO: Document
restoreSnapshot :: MonadIO m => ConsulClient -> m ()
restoreSnapshot = undefined
