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
  , aclTokenReadById
  , aclTokenReadSelf
  , aclTokenUpdate
  ) where

import Import

aclTokenClone = undefined

-- | TODO
aclTokenCreate
  :: ConsulClient -- ^
  -> ConsulApiRequestAclTokenCreate
  -> IO (Either String AclToken) -- ^
aclTokenCreate client@ConsulClient{..} token =
  consulHttpPutHelper client "/v1/acl/token" token


-- | This endpoint deletes an existing ACL Token.
--
aclTokenDelete
  :: ConsulClient -- ^
  -> Text -- TODO: UUID
  -> IO (Either String Bool) -- ^
aclTokenDelete client@ConsulClient{..} id =  do
  consulHttpDeleteHelper client ("/v1/acl/token/" <> id)


-- | This endpoint lists (all?) ACL tokens.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclTokenList
  :: ConsulClient -- ^
  -> IO (Either String [AclToken]) -- ^
aclTokenList client@ConsulClient{..} =
  consulHttpGetHelper client "/v1/acl/tokens"


-- | This endpoint reads an existing ACL token with the given Id.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclTokenReadById
  :: ConsulClient -- ^
  -> Text -- ^ TODO: AccessorId (UUID)
  -> IO (Either String AclToken) -- ^
aclTokenReadById client id =  do
  consulHttpGetHelper client ("/v1/acl/token/" <> id)

-- | This endpoint returns the ACL token details that matches the secret ID
--   specified with the X-Consul-Token header or the token query parameter.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclTokenReadSelf
  :: ConsulClient -- ^
  -> IO (Either String AclToken) -- ^
aclTokenReadSelf client =  do
  consulHttpGetHelper client "/v1/acl/token/self/"


-- | This endpoint updates an existing ACL Token.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclTokenUpdate
  :: ConsulClient -- ^
  -> AclToken
  -> IO (Either String AclToken) -- ^
aclTokenUpdate client token = -- do
  consulHttpPutHelper client ("/v1/acl/token/" <> (aclTokenAccessorId token)) token

