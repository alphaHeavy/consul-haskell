{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | The functions in this module correspond to
the [Consul Acl Policy API](https://www.consul.io/api-docs/acl/policies).

This module is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

-}

module Network.Consul.Client.AclPolicy
  ( aclPolicyCreate
  , aclPolicyDelete
  , aclPolicyList
  , aclPolicyRead
  , aclPolicyUpdate
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
-- TODO: support token, improve the client initialization?
aclPolicyCreate
  :: ConsulClient -- ^
  -> ConsulApiRequestAclPolicyCreate
  -> IO (Either String AclPolicy) -- ^
aclPolicyCreate client@ConsulClient{..} policy =  do
  let hostnameWithScheme = hostWithScheme client
  initReq <-
    liftIO $
      parseUrlThrow $
        T.unpack $
          T.concat
            [ hostnameWithScheme
            , ":"
            , T.pack $ show ccPort
            ,"/v1/acl/policy"
            ]
  let tokenHeader =
        case ccToken of 
          Nothing -> []
          Just token -> [(hAuthorization, (encodeUtf8 ("Bearer " <> token)))]
  let request = 
        initReq
          { method = "PUT"
          , requestBody = (RequestBodyLBS $ encode policy)
          , requestHeaders = tokenHeader
          }
  liftIO $ withResponse request ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ eitherDecode $ BL.fromStrict body


aclPolicyDelete = undefined
aclPolicyList = undefined
aclPolicyRead = undefined
aclPolicyUpdate = undefined


