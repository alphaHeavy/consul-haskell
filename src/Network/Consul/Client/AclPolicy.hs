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
  , aclPolicyReadById
  , aclPolicyReadByName
  , aclPolicyUpdate
  ) where

import Import

-- | This endpoint creates a new ACL policy.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclPolicyCreate
  :: ConsulClient -- ^
  -> ConsulApiRequestAclPolicyCreate
  -> IO (Either String AclPolicy) -- ^
aclPolicyCreate client@ConsulClient{..} policy =
  consulHttpPutHelper client "/v1/acl/policy" policy


-- | This endpoint deletes an existing ACL policy.
--
aclPolicyDelete
  :: ConsulClient -- ^
  -> Text -- TODO: UUID
  -> IO (Either String Bool) -- ^
aclPolicyDelete client@ConsulClient{..} id =  do
  consulHttpDeleteHelper client ("/v1/acl/policy/" <> id)


-- | This endpoint lists (all?) ACL policies.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclPolicyList
  :: ConsulClient -- ^
  -> IO (Either String [AclPolicy]) -- ^
aclPolicyList client@ConsulClient{..} = -- do
  consulHttpGetHelper client "/v1/acl/policies"


aclPolicyUpdate
  :: ConsulClient -- ^
  -> AclPolicy
  -> IO (Either String AclPolicy) -- ^
aclPolicyUpdate client policy = -- do
  consulHttpPutHelper client ("/v1/acl/policy/" <> (aclPolicyId policy)) policy




-- | This endpoint reads an existing ACL policy with the given Id.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclPolicyReadById
  :: ConsulClient -- ^
  -> Text -- ^ TODO: UUID
  -> IO (Either String AclPolicy) -- ^
aclPolicyReadById client id =  do
  consulHttpGetHelper client ("/v1/acl/policy/" <> id)


-- | This endpoint reads an existing ACL policy with the given name.
--
-- TODO: support ns query parameter (namespace, enterprise-only)
--
aclPolicyReadByName
  :: ConsulClient -- ^
  -> Text -- ^
  -> IO (Either String AclPolicy) -- ^
aclPolicyReadByName client name = -- do
  consulHttpGetHelper client ("/v1/acl/policy/name/" <> name)


