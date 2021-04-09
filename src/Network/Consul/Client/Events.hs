{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Client.Events
  ( createEvent
  , listEvents
  ) where

import Import

-- | TODO: Document
createEvent :: MonadIO m => ConsulClient -> SessionRequest -> m ()
createEvent = undefined


-- | TODO: Document
listEvents :: MonadIO m => ConsulClient -> SessionRequest -> m ()
listEvents = undefined
