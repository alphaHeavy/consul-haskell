{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Consul (
  -- Catalog
    deregisterService
  , getDatacenters
  , getService
  , getServices
  , registerService

  -- Health
  , deregisterHealthCheck
  , failHealthCheck
  , registerHealthCheck
  , warnHealthCheck
  , getSelf
  , getServiceChecks
  , getServiceHealth
  , passHealthCheck

  , getSequencerForLock
  , initializeConsulClient
  , initializeTlsConsulClient
  , isValidSequencer
  , runService
  , withSequencer

  --Agent

  -- KV Store API
  , deleteKey
  , getKey
  , getKeys
  , listKeys
  , putKey
  , putKeyAcquireLock
  , putKeyReleaseLock

  -- Session API
  , createSession
  , destroySession
  , getSessionInfo
  , renewSession
  , withSession

  , module Network.Consul.Types
) where

import Import

import Network.Consul.Types
-- Consul Client APIs
import Network.Consul.Client.Acl
import Network.Consul.Client.Agent
import Network.Consul.Client.Catalog
import Network.Consul.Client.Config
import Network.Consul.Client.Connect
import Network.Consul.Client.Events
import Network.Consul.Client.Health
import Network.Consul.Client.Init
import Network.Consul.Client.KVStore
import Network.Consul.Client.Namespaces
import Network.Consul.Client.Operator
import Network.Consul.Client.PreparedQueries
import Network.Consul.Client.Session
import Network.Consul.Client.Snapshots
import Network.Consul.Client.Status
import Network.Consul.Client.Transactions

import Network.Consul.Sequencer
import Network.Consul.Misc

import Prelude hiding (mapM)
