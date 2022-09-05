{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | The functions in this module correspond to
the [Consul Acl Role API](https://www.consul.io/api-docs/acl/roles).

This module is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

-}

module Network.Consul.Client.AclRole
  ( aclRoleCreate
  , aclRoleDelete
  , aclRoleList
  , aclRoleReadById
  , aclRoleReadByName
  , aclRoleUpdate
  ) where

import Import


-- | TODO
aclRoleCreate
  :: ConsulClient -- ^
  -> ConsulApiRequestAclRoleCreate
  -> IO (Either String AclRole) -- ^
aclRoleCreate client@ConsulClient{..} role =
  consulHttpPutHelper client "/v1/acl/role" role


-- | This endpoint deletes an existing ACL Role.
--
aclRoleDelete
  :: ConsulClient -- ^
  -> Text -- TODO: UUID
  -> IO (Either String Bool) -- ^
aclRoleDelete client@ConsulClient{..} id =  do
  consulHttpDeleteHelper client ("/v1/acl/role/" <> id)


-- | This endpoint lists (all?) ACL roles.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclRoleList
  :: ConsulClient -- ^
  -> IO (Either String [AclRole]) -- ^
aclRoleList client@ConsulClient{..} =
  consulHttpGetHelper client "/v1/acl/roles"


-- | This endpoint reads an existing ACL role with the given Id.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclRoleReadById
  :: ConsulClient -- ^
  -> Text -- ^ TODO: UUID
  -> IO (Either String AclRole) -- ^
aclRoleReadById client id =  do
  consulHttpGetHelper client ("/v1/acl/role/" <> id)

-- | This endpoint reads an existing ACL role with the given Name.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclRoleReadByName
  :: ConsulClient -- ^
  -> Text -- ^ TODO: UUID
  -> IO (Either String AclRole) -- ^
aclRoleReadByName client name =  do
  consulHttpGetHelper client ("/v1/acl/role/name/" <> name)


-- | This endpoint updates an existing ACL Role.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclRoleUpdate
  :: ConsulClient -- ^
  -> AclRole
  -> IO (Either String AclRole) -- ^
aclRoleUpdate client role = -- do
  consulHttpPutHelper client ("/v1/acl/role/" <> (aclRoleId role)) role

