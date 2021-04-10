{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
-- https://www.consul.io/api-docs/coordinate
module Network.Consul.Client.Coordinates
  ( getNodeLANCoordinates
  , listNodeLANCoordinates
  , listServerWANCoordinates
  , updateNodeLANCoordinates
  ) where

import Import

-- | Read LAN Coordinates for a node
getNodeLANCoordinates :: MonadIO m => ConsulClient -> m ()
getNodeLANCoordinates = undefined

-- | Read LAN Coordinates for all nodes
listNodeLANCoordinates :: MonadIO m => ConsulClient -> m ()
listNodeLANCoordinates = undefined

-- | Read WAN Coordinates
-- > This endpoint returns the WAN network coordinates for all Consul servers,
-- > organized by datacenters. It serves data out of the server's local Serf data,
-- > so its results may vary as requests are handled by different servers in the
-- > cluster.
listServerWANCoordinates :: MonadIO m => ConsulClient -> m ()
listServerWANCoordinates = undefined

-- | Update LAN Coordinates for a node
updateNodeLANCoordinates :: MonadIO m => ConsulClient -> m ()
updateNodeLANCoordinates = undefined

