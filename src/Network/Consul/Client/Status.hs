{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Client.Status
  ( getRaftPeer
  , listRaftPeers
  ) where

import Import

-- | Get Raft Peer
--getRaftPeer :: MonadIO m => ConsulClient -> Maybe Datacenter -> m (Maybe Peer)
getRaftPeer :: MonadIO m => ConsulClient -> m ()
getRaftPeer = undefined

-- | List Raft Peers
--listRaftPeers :: MonadIO m => ConsulClient -> Maybe Datacenter -> m ([Peer])
listRaftPeers :: MonadIO m => ConsulClient -> m ()
listRaftPeers = undefined
