{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | The functions in this module correspond to
the [Consul Acl Token API](https://www.consul.io/api-docs/acl/tokens).

This module is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

-}

module Network.Consul.Client.AclToken
  ( aclTokenClone
  , aclTokenCreate
  , aclTokenDelete
  , aclTokenList
  , aclTokenRead
  , aclTokenReadSelf
  , aclTokenUpdate
  ) where

import Import

import qualified Data.ByteString as B (concat) 
import qualified Data.ByteString.Lazy as BL (fromStrict)
import qualified Data.Text as T (concat, pack, unpack)

import Data.Aeson (eitherDecode)


-- | This endpoint creates a new ACL token.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclTokenCreate
  :: ConsulClient -- ^
  -> ConsulApiRequestAclTokenCreate
  -> IO (Either String ConsulApiResponseAclCheckReplication) -- ^
aclTokenCreate client@ConsulClient{..} tokenRequest =  do
  let hostnameWithScheme = hostWithScheme client
  initReq <-
    liftIO $
      parseUrlThrow $
        T.unpack $
          T.concat
            [ hostnameWithScheme
            , ":"
            , T.pack $ show ccPort
            ,"/v1/acl/token"
            ]
  let request = 
        initReq
          { method = "PUT"
          , requestBody = (RequestBodyLBS $ encode tokenRequest)
          }
  liftIO $ withResponse request ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ eitherDecode $ BL.fromStrict body

aclTokenClone = undefined
aclTokenDelete = undefined
aclTokenList = undefined
aclTokenRead = undefined
aclTokenReadSelf = undefined
aclTokenUpdate = undefined
