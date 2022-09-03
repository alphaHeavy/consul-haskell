{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consul.AclTokenSpec where

import Import

testsuiteSettingsWithAclsEnabled = 
  TestsuiteSettings
    { displayConsulServerLogs = False -- True
    , enableAcls = True
    , verboseLogs = False -- True
    }

spec :: Spec
spec = setupAround (consulServerSetupFuncWith testsuiteSettingsWithAclsEnabled) $ do

  it "AclTokenCreate" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclTokenCreate(Bootstrap acls): failed " ++ e)
      Right aclBootstrapResponse -> do
        let token = consulApiResponseAclBootstrapSecretId aclBootstrapResponse
        secondClient@ConsulClient{..} <- newClient $ (consulServerHandleHttpPort consulServerHandle)
                                                  
        context "AclTokenCreate: successful" $ pure ()



