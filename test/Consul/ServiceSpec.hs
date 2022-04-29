{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consul.ServiceSpec (spec) where

import Import
import Data.Maybe (isJust)

import Test.Syd (modifyMaxSuccess)
import Test.Syd.Validity
import Data.GenValidity.Text --()

spec :: Spec
spec = setupAround consulServerSetupFunc $ modifyMaxSuccess (`div` 20) $ do

  it "testRegisterService" $ \consulServerHandle ->
    forAllValid $ \serviceName -> do
      let consulServerPort = consulServerHandleHttpPort consulServerHandle
      let consulServerPort = consulServerHandleHttpPort consulServerHandle
      client@ConsulClient{..} <- newClient consulServerPort
      let req = RegisterService Nothing "testService" [serviceName] Nothing (Just $ Ttl "10s")
      val <- registerService client req
      context "testRegisterService: Service was not created" $ val `shouldBe` True
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
    context "testDeregisterService: Service was not created" $ val `shouldBe` True
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
    context "testGetSelf: Self not returned" $ (isJust x) `shouldBe` True



