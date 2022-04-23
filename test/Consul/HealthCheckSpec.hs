{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Consul.HealthCheckSpec (spec) where

import Import
import Test.Syd

spec :: Spec
spec = setupAround consulServerSetupFunc $ do

  it "testGetServiceHealth" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle 
    let req = RegisterService (Just "testGetServiceHealth") "testGetServiceHealth" [] Nothing Nothing
    r1 <- registerService client req
    case r1 of
      True -> do
        liftIO $ sleep 1
        r2 <- getServiceHealth client "testGetServiceHealth"
        case r2 of
          Just [x] -> return ()
          Just [] -> expectationFailure "testGetServiceHealth: No Services Returned"
          Nothing -> expectationFailure "testGetServiceHealth: Failed to parse result"
      False -> expectationFailure "testGetServiceHealth: Service was not created"

--{- Health Checks -}
--itWithOuter "testRegisterHealthCheck" $ \_ ->  do
--  client@ConsulClient{..} <- newClient
--  let check = RegisterHealthCheck "testHealthCheck" "testHealthCheck" "" Nothing Nothing (Just "15s")
--  x1 <- registerHealthCheck ccManager (hostWithScheme client) ccPort check

