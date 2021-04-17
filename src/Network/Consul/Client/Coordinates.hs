{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The functions in this module correspond to
the [Consul Coordinate API](https://www.consul.io/api/coordinate).

This module is a WIP, please feel free to contribute via the repo on GitHub.

__Missing Functions__

* `getNodeLANCoordinates`
* `listNodeLANCoordinates`
* `listServerWANCoordinates`
* `updateNodeLANCoordinates`
-}
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

