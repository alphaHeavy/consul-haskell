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


-- | TODO
aclTokenCreate
  :: ConsulClient -- ^
  -> ConsulApiRequestAclTokenCreate
  -> IO (Either String AclToken) -- ^
aclTokenCreate client@ConsulClient{..} token =
  consulHttpPutHelper client "/v1/acl/token" token



aclTokenClone = undefined
aclTokenDelete = undefined
aclTokenList = undefined
aclTokenRead = undefined
aclTokenReadSelf = undefined
aclTokenUpdate = undefined
