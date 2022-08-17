{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consul.AclSpec where

import Import

spec :: Spec
spec = setupAround consulServerSetupFunc $ do

  it "BootstrapAcls" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    -- specify the datacenter as part of our request
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("Bootstrap acls: failed " ++ e)
      Right aclBootstrapResponse -> do
        context "Bootstrap acls: successful" $ pure ()


