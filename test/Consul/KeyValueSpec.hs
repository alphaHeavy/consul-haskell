{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Consul.KeyValueSpec where

import Import

import Test.Syd (modifyMaxSuccess)
import Test.Syd.Validity
import Data.GenValidity.ByteString --()
import Data.Text (pack)

spec :: Spec
spec = setupAround consulServerSetupFunc $ modifyMaxSuccess (`div` 20) $ do

  it "Get Invalid Key" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    -- specify the datacenter as part of our request
    x <- getKey client{ ccDatacenter = dc1  } "nokey" Nothing Nothing
    context "testGetInvalidKey: Found a key that doesn't exist" $ x `shouldBe` Nothing

  it "testPutKey" $ \consulServerHandle ->
    forAllValid $ \keyContents -> do
      client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
      let put = KeyValuePut "/testPutKey" keyContents Nothing Nothing
      x <- putKey client put
      context "testPutKey: Write failed" $ x `shouldBe` True

  it "testPutKeyAcquireLock" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let nodeName = consulServerHandleNodeName consulServerHandle
        ttl = "30s"
        req =
          SessionRequest
            lockDelay
            (Just "testPutKeyAcquireLock")
            nodeName
            checkIds
            (Just Release)
            (Just ttl)
    result <- createSession client req
    case result of
      Nothing -> expectationFailure "testPutKeyAcquireLock: No session was created"
      Just session -> do
        let put = KeyValuePut "/testPutKeyAcquireLock" "Test" Nothing Nothing
        x <- putKeyAcquireLock client put session
        context "testPutKeyAcquireLock: Write failed" $ x `shouldBe` True
        Just kv <- getKey client "/testPutKeyAcquireLock" Nothing Nothing
        let Just returnedSession = kvSession kv
        context "testPutKeyAcquireLock: Session was not found on key" $ returnedSession `shouldBe` (sId session)


  it "testPutKeyReleaseLock" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let ttl = "30s"
        nodeName = consulServerHandleNodeName consulServerHandle
        req =
          SessionRequest
            Nothing
            (Just "testPutKeyReleaseLock")
            nodeName
            checkIds
            (Just Release)
            (Just ttl)
    result <- createSession client req
    case result of
      Nothing -> expectationFailure "testPutKeyReleaseLock: No session was created"
      Just session -> do
        let put = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
        x <- putKeyAcquireLock client put session
        context "testPutKeyReleaseLock: Write failed" $ x `shouldBe` True
        Just kv <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
        let Just returnedSession = kvSession kv
        context "testPutKeyReleaseLock: Session was not found on key" $ returnedSession `shouldBe` (sId session)
        let put2 = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
        x2 <- putKeyReleaseLock client put2 session
        context "testPutKeyReleaseLock: Release failed" $ x2 `shouldBe` True
        Just kv2 <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
        context "testPutKeyAcquireLock: Session still held" $ (kvSession kv2) `shouldBe` Nothing


  it "testGetKey" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let put = KeyValuePut "/testGetKey" "Test" Nothing Nothing
    x1 <- putKey client put
    context "testGetKey: Write failed" $ x1 `shouldBe` True
    x2 <- getKey client "/testGetKey" Nothing Nothing
    case x2 of
      Just x -> context "testGetKey: Incorrect Value" $ (kvValue x) `shouldBe` (Just "Test")
      Nothing -> expectationFailure "testGetKey: No value returned"

  it "testGetNullValueKey" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let put = KeyValuePut "/testGetNullValueKey" "" Nothing Nothing
    x1 <- putKey client put
    context "testGetNullValueKey: Write failed" $ x1 `shouldBe` True
    liftIO $ sleep 0.5
    x2 <- getKey client "/testGetNullValueKey" Nothing Nothing
    case x2 of
      Just x -> context "testGetNullValueKey: Incorrect Value" $ (kvValue x) `shouldBe` Nothing
      Nothing -> expectationFailure "testGetNullValueKey: No value returned"

  it "testGetKeys" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let put1 = KeyValuePut "/testGetKeys/key1" "Test" Nothing Nothing
    x1 <- putKey client put1
    context "testGetKeys: Write failed" $ x1 `shouldBe` True
    let put2 = KeyValuePut "/testGetKeys/key2" "Test" Nothing Nothing
    x2 <- putKey client put2
    context "testGetKeys: Write failed" $ x2 `shouldBe` True
    x3 <- getKeys client "/testGetKeys" Nothing Nothing
    context "testGetKeys: Incorrect number of results" $ (length x3) `shouldBe` 2

  it "testListKeys" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let put1 = KeyValuePut "/testListKeys/key1" "Test" Nothing Nothing
    x1 <- putKey client put1
    context "testListKeys: Write failed" $ x1 `shouldBe` True
    let put2 = KeyValuePut "/testListKeys/key2" "Test" Nothing Nothing
    x2 <- putKey client put2
    context "testListKeys: Write failed" $ x2 `shouldBe` True
    x3 <- listKeys client "/testListKeys/" Nothing Nothing
    context "testListKeys: Incorrect number of results" $ (length x3) `shouldBe` 2

  it "testDeleteKey" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let put1 = KeyValuePut "/testDeleteKey" "Test" Nothing Nothing
    x1 <- putKey client put1
    context "testDeleteKey: Write failed" $ x1 `shouldBe` True
    x2 <- deleteKey client "/testDeleteKey" False
    context "testDeleteKey: Delete Failed" $ x2 `shouldBe` True
    x3 <- getKey client "/testDeleteKey" Nothing Nothing
    context "testDeleteKey: Key was not deleted" $ x3 `shouldBe` Nothing

  it "testDeleteRecursive" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let put1 = KeyValuePut "/testDeleteRecursive/1" "Test" Nothing Nothing
        put2 = KeyValuePut "/testDeleteRecursive/2" "Test" Nothing Nothing
    x1 <- putKey client put1
    context "testDeleteKey: Write failed" $ x1 `shouldBe` True
    x2 <- putKey client put2
    context "testDeleteKey: Write failed" $ x2 `shouldBe` True
    deleteKey client "/testDeleteRecursive/" True
    x3 <- getKey client "/testDeleteRecursive/1" Nothing Nothing
    context "testDeleteKey: Key was not deleted" $ x3 `shouldBe` Nothing
