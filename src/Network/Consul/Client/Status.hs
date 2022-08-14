{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The functions in this module correspond to
the [Consul Status API](https://www.consul.io/api/status).

Feel free to contribute via the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

__Missing Functions__

* `getRaftPeer`
* `listRaftPeers`
-}
module Network.Consul.Client.Status
  ( getRaftPeer
  , listRaftPeers
  ) where

import Import

{- | Get Raft Peer

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
--getRaftPeer :: MonadIO m => ConsulClient -> Maybe Datacenter -> m (Maybe Peer)
getRaftPeer :: MonadIO m => ConsulClient -> m ()
getRaftPeer = undefined

{- | List Raft Peers

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
--listRaftPeers :: MonadIO m => ConsulClient -> Maybe Datacenter -> m ([Peer])
listRaftPeers :: MonadIO m => ConsulClient -> m ()
listRaftPeers = undefined
