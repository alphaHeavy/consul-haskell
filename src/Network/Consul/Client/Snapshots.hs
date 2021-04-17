{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The functions in this module correspond to
the [Consul Snapshot API](https://www.consul.io/api/snapshot).

Feel free to contribute via the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

__Missing Functions__

* `createSnapshot`
* `restoreSnapshot`
-}
module Network.Consul.Client.Snapshots
  ( createSnapshot -- generateSnapshot
  , restoreSnapshot
  ) where

import Import


{- | Create Snapshot

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
createSnapshot :: MonadIO m => ConsulClient -> m ()
createSnapshot = undefined


{- | Restore Snapshot

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
restoreSnapshot :: MonadIO m => ConsulClient -> m ()
restoreSnapshot = undefined
