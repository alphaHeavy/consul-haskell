{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consul.AgentSpec where

import Import

spec :: Spec
spec = setupAround consulServerSetupFunc $ do

  it "getSelf returns something" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    -- specify the datacenter as part of our request
    self <- getSelf client
    case self of
      Nothing -> expectationFailure "getSelf: failed, nothing returned!"
      Just x -> do
        --if debug then (print self)
        context "getSelf: successful" $ pure ()


