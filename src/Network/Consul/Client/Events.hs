{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The functions in this module correspond to
the [Consul Event API](https://www.consul.io/api/event).

This module is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

__Missing Functions__

* `createEvent`
* `listEvents`
-}
module Network.Consul.Client.Events
  ( createEvent
  , listEvents
  ) where

import Import

{- | Create (fire) a new Event

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
createEvent :: MonadIO m => ConsulClient -> SessionRequest -> m ()
createEvent = undefined


{- | List Recent Events

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
listEvents :: MonadIO m => ConsulClient -> SessionRequest -> m ()
listEvents = undefined
