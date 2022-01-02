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
import qualified Data.ByteString as B (concat) 
import qualified Data.ByteString.Lazy as BL (toStrict, fromStrict)
import qualified Data.Text as T (concat)


-- | Read LAN Coordinates for a node
-- TODO: switch Text --> NodeName
-- TODO: document
--
-- @since 0.0.0.0
getNodeLANCoordinates
  :: MonadIO m => Text -- ^
  -> ConsulClient -- ^
  -> m [NodeCoordinates]
getNodeLANCoordinates nodeName _client@ConsulClient{..} = do
  let hostnameWithScheme = hostWithScheme _client
  req <- createRequest hostnameWithScheme
           ccPort
           apiUrl
           Nothing
           Nothing
           False
           ccDatacenter
  liftIO $ withResponse req ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let val = (decode $ BL.fromStrict body)
    case val of
      Just x -> return x
      Nothing -> return []
  where
    apiUrl = (T.concat ["/v1/coordinate/node/", nodeName])
  

-- | Read LAN Coordinates for all nodes
-- TODO: confirm this works
--
-- @since 0.0.0.0
listNodeLANCoordinates
  :: MonadIO m => ConsulClient -- ^
  -> m [NodeCoordinates] -- ^
listNodeLANCoordinates _client@ConsulClient{..} = do
  let hostnameWithScheme = hostWithScheme _client
  req <- createRequest hostnameWithScheme
           ccPort
           apiUrl
           Nothing
           Nothing
           False
           ccDatacenter
  liftIO $ withResponse req ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let val = (decode $ BL.fromStrict body)
    case val of
      Just x -> return x
      Nothing -> return []
  where
    apiUrl = "/v1/coordinate/nodes/"
  

-- | Read WAN Coordinates
-- > This endpoint returns the WAN network coordinates for all Consul servers,
-- > organized by datacenters. It serves data out of the server's local Serf data,
-- > so its results may vary as requests are handled by different servers in the
-- > cluster.
--
-- TODO: confirm this works
--
-- @since 0.0.0.0
listServerWANCoordinates
  :: MonadIO m => ConsulClient -- ^
  -> m [WANCoordinates] -- ^
listServerWANCoordinates _client@ConsulClient{..} = do
  let hostnameWithScheme = hostWithScheme _client
  req <- createRequest hostnameWithScheme
           ccPort
           "/v1/coordinate/datacenters/"
           Nothing
           Nothing
           False
           ccDatacenter
  liftIO $ withResponse req ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let val = (decode $ BL.fromStrict body)
    case val of
      Just x -> return x
      Nothing -> return []


-- | Update LAN Coordinates for a node
-- > This endpoint updates the LAN network coordinates for a node in a given datacenter.
-- https://www.consul.io/api-docs/coordinate#update-lan-coordinates-for-a-node
--
-- TODO: confirm this works
--
-- @since 0.0.0.0
updateNodeLANCoordinates
  :: MonadIO m => ConsulClient -- ^
  -> NodeCoordinates -- ^
  -> m Bool -- ^
updateNodeLANCoordinates client request = do
  let portNumber = ccPort client
      manager = ccManager client
      hostname = hostWithScheme client
      dc = ccDatacenter client
  initReq <- createRequest hostname
                           portNumber
                           "/v1/coordinate/update"
                           Nothing
                           (Just $ BL.toStrict $ encode request)
                           False
                           dc
  liftIO $ withResponse initReq manager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> return True
      _ -> return False

