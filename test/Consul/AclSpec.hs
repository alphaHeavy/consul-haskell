{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consul.AclSpec where

import Import

testsuiteSettingsWithAclsEnabled = 
  TestsuiteSettings
    { displayConsulServerLogs = False -- True
    , enableAcls = True
    , verboseLogs = True -- False
    }

spec :: Spec
spec = setupAround (consulServerSetupFuncWith testsuiteSettingsWithAclsEnabled) $ do

  -- TODO: why does this operation fail with leader unable to find and remove the acl-reset-bootstrap
  -- agent.server.acl: failed to remove bootstrap file: error="remove
  -- /run/user/1001/consul-server-e96ac147c2a743a8/acl-bootstrap-reset: no such file or directory"
  it "BootstrapAcls" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    -- specify the datacenter as part of our request
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("Bootstrap acls: failed " ++ e)
      Right aclBootstrapResponse -> do
        print aclBootstrapResponse
        context "Bootstrap acls: successful" $ pure ()




  it "CheckReplication" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    -- specify the datacenter as part of our request
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("CheckReplication, bootstrap acls: failed " ++ e)
      Right aclBootstrapResponse -> do
        statusResponse <- aclCheckReplication client{ ccDatacenter = dc1 }
        case statusResponse of
          Left e -> expectationFailure ("CheckReplication: failed" ++ e)
          Right aclCheckReplicationResponse -> do
            context "CheckReplication: successful" $ pure ()

