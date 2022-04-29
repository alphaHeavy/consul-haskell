{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consul.ServiceSpec (spec) where

import Import
-- why isn't isJust coming from the import in Import?
import Data.Maybe (isJust)

spec :: Spec
spec = setupAround consulServerSetupFunc $ do

  it "testRegisterService" $ \consulServerHandle -> do
    let consulServerPort = consulServerHandleHttpPort consulServerHandle
    let consulServerPort = consulServerHandleHttpPort consulServerHandle
    client@ConsulClient{..} <- newClient consulServerPort
    let req = RegisterService Nothing "testService" ["test"] Nothing (Just $ Ttl "10s")
    val <- registerService client req
    context "testRegisterService: Service was not created" $ shouldBe val True
    mService <- getService client "testService" Nothing
    let serviceWasNotFound = expectationFailure "testRegisterService: Service was not found"
    case mService of
      Just [] -> serviceWasNotFound
      Nothing -> serviceWasNotFound
      Just _ -> return ()

  it "testDeregisterService" $ \consulServerHandle -> do
    let consulServerPort = consulServerHandleHttpPort consulServerHandle
    client@ConsulClient{..} <- newClient consulServerPort
    let req = RegisterService Nothing "testService" ["test"] Nothing (Just $ Ttl "30s")
    val <- registerService client req
    context "testDeregisterService: Service was not created" $ shouldBe val True
    deregisterService client (rsName req)
    mService <- getService client (rsName req) Nothing
    case mService of
      Just [] -> return ()
      Nothing -> return ()
      Just s -> expectationFailure ("testDeregisterService: Service was found... " <> show s)

  it "testGetSelf" $ \consulServerHandle -> do
    let consulServerPort = consulServerHandleHttpPort consulServerHandle
    client@ConsulClient{..} <- newClient consulServerPort
    x <- getSelf client
    context "testGetSelf: Self not returned" $ shouldBe True (isJust x)



