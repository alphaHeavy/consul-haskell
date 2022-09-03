{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The functions in this module correspond to
the [Consul Acl API](https://www.consul.io/api/acl).

This module is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

__Missing Functions__

* All.
-}
module Network.Consul.Client.Acl
  ( aclBootstrap 
  , aclCheckReplication
  ) where

import Import
import qualified Data.ByteString as B (concat) 
import qualified Data.ByteString.Lazy as BL (fromStrict)
import qualified Data.Text as T (concat, pack, unpack)

import Data.Aeson (eitherDecode)

--
-- Upstream docs
--
-- https://www.consul.io/api-docs/acl
--
--
{- | Bootstrap
--
-- `aclBootstrap` corresponds to the
-- [Bootstrap ACLs](https://www.consul.io/api-docs/acl#bootstrap-acls) API.
--
-- From those docs:
--
-- > You can detect if something has interfered with the ACL bootstrapping process
-- > by checking the response code. A 200 response means that the bootstrap was a
-- > success, and a 403 means that the cluster has already been bootstrapped, at
-- > which point you should consider the cluster in a potentially compromised state.
-- 
-- > The returned token will have unrestricted privileges to manage all details of
-- > the system. It can then be used to further configure the ACL system. Please
-- > check the ACL tutorial for more details.
-}

-- data ConsulAclResponse
--   = Response200 ConsulApiResponseAclBootstrap
--   | Response401 Text
--   | Response404 Text

-- TODO: capture and return response code..
-- the http api fails with 403 if the bootstrap has already been run
aclBootstrap
  :: ConsulClient -- ^
  -> IO (Either String ConsulApiResponseAclBootstrap) -- ^
aclBootstrap client@ConsulClient{..} =  do
  let hostnameWithScheme = hostWithScheme client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/acl/bootstrap"]
  liftIO $ withResponse (initReq {method = "PUT"} ) ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ eitherDecode $ BL.fromStrict body


-- | This endpoint returns the status of the ACL replication processes in the
--   datacenter. This is intended to be used by operators or by automation
--   checking to discover the health of ACL replication.
--
-- TODO: include dc as a query parameter, with maybe?
--
aclCheckReplication
  :: ConsulClient -- ^
  -> IO (Either String ConsulApiResponseAclCheckReplication) -- ^
aclCheckReplication client@ConsulClient{..} =  do
  let hostnameWithScheme = hostWithScheme client
  initReq <-
    liftIO $
      parseUrlThrow $
        T.unpack $
          T.concat
            [ hostnameWithScheme
            , ":"
            , T.pack $ show ccPort
            ,"/v1/acl/replication"
            ]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ eitherDecode $ BL.fromStrict body

