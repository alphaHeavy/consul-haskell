{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Consul.KeyValueSpec where

import Import
import Test.Syd

spec :: Spec
spec = setupAroundAll consulServerSetupFunc $ do

  itWithOuter "Get Invalid Key" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    -- specify the datacenter as part of our request
    x <- getKey client{ ccDatacenter = dc1  } "nokey" Nothing Nothing
    context "testGetInvalidKey: Found a key that doesn't exist" $ shouldBe x Nothing

  itWithOuter "testPutKey" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    let put = KeyValuePut "/testPutKey" "Test" Nothing Nothing
    x <- putKey client put
    context "testPutKey: Write failed" $ shouldBe True x

  itWithOuter "testPutKeyAcquireLock" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    let ttl = "30s"
        req =
          SessionRequest
            lockDelay
            (Just "testPutKeyAcquireLock")
            localNode
            checkIds
            (Just Release)
            (Just ttl)
    result <- createSession client req
    case result of
      Nothing -> expectationFailure "testPutKeyAcquireLock: No session was created"
      Just session -> do
        let put = KeyValuePut "/testPutKeyAcquireLock" "Test" Nothing Nothing
        x <- putKeyAcquireLock client put session
        context "testPutKeyAcquireLock: Write failed" $ shouldBe  True x
        Just kv <- getKey client "/testPutKeyAcquireLock" Nothing Nothing
        let Just returnedSession = kvSession kv
        context "testPutKeyAcquireLock: Session was not found on key" $ shouldBe returnedSession (sId session)


  itWithOuter "testPutKeyReleaseLock" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    let ttl = "30s"
        req =
          SessionRequest
            Nothing
            (Just "testPutKeyReleaseLock")
            localNode
            checkIds
            (Just Release)
            (Just ttl)
    result <- createSession client req
    case result of
      Nothing -> expectationFailure "testPutKeyReleaseLock: No session was created"
      Just session -> do
        let put = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
        x <- putKeyAcquireLock client put session
        context "testPutKeyReleaseLock: Write failed" $ shouldBe True x
        Just kv <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
        let Just returnedSession = kvSession kv
        context "testPutKeyReleaseLock: Session was not found on key" $ shouldBe returnedSession (sId session)
        let put2 = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
        x2 <- putKeyReleaseLock client put2 session
        context "testPutKeyReleaseLock: Release failed" $ shouldBe True x2
        Just kv2 <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
        context "testPutKeyAcquireLock: Session still held" $ shouldBe Nothing (kvSession kv2)


  itWithOuter "testGetKey" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    let put = KeyValuePut "/testGetKey" "Test" Nothing Nothing
    x1 <- putKey client put
    context "testGetKey: Write failed" $ shouldBe True x1
    x2 <- getKey client "/testGetKey" Nothing Nothing
    case x2 of
      Just x -> context "testGetKey: Incorrect Value" $ shouldBe (kvValue x) (Just "Test")
      Nothing -> expectationFailure "testGetKey: No value returned"

  itWithOuter "testGetNullValueKey" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    let put = KeyValuePut "/testGetNullValueKey" "" Nothing Nothing
    x1 <- putKey client put
    context "testGetNullValueKey: Write failed" $ shouldBe True x1
    liftIO $ sleep 0.5
    x2 <- getKey client "/testGetNullValueKey" Nothing Nothing
    case x2 of
      Just x -> context "testGetNullValueKey: Incorrect Value" $ shouldBe (kvValue x) Nothing
      Nothing -> expectationFailure "testGetNullValueKey: No value returned"

  itWithOuter "testGetKeys" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    let put1 = KeyValuePut "/testGetKeys/key1" "Test" Nothing Nothing
    x1 <- putKey client put1
    context "testGetKeys: Write failed" $ shouldBe True x1
    let put2 = KeyValuePut "/testGetKeys/key2" "Test" Nothing Nothing
    x2 <- putKey client put2
    context "testGetKeys: Write failed" $ shouldBe True x2
    x3 <- getKeys client "/testGetKeys" Nothing Nothing
    context "testGetKeys: Incorrect number of results" $ shouldBe 2 (length x3)

  itWithOuter "testListKeys" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    let put1 = KeyValuePut "/testListKeys/key1" "Test" Nothing Nothing
    x1 <- putKey client put1
    context "testListKeys: Write failed" $ shouldBe True x1
    let put2 = KeyValuePut "/testListKeys/key2" "Test" Nothing Nothing
    x2 <- putKey client put2
    context "testListKeys: Write failed" $ shouldBe True x2
    x3 <- listKeys client "/testListKeys/" Nothing Nothing
    context "testListKeys: Incorrect number of results" $ shouldBe 2 (length x3)

  itWithOuter "testDeleteKey" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    let put1 = KeyValuePut "/testDeleteKey" "Test" Nothing Nothing
    x1 <- putKey client put1
    context "testDeleteKey: Write failed" $ shouldBe True x1
    x2 <- deleteKey client "/testDeleteKey" False
    context "testDeleteKey: Delete Failed" $ shouldBe True x2
    x3 <- getKey client "/testDeleteKey" Nothing Nothing
    context "testDeleteKey: Key was not deleted" $ shouldBe Nothing x3

  itWithOuter "testDeleteRecursive" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandlePort consulServerHandle
    let put1 = KeyValuePut "/testDeleteRecursive/1" "Test" Nothing Nothing
        put2 = KeyValuePut "/testDeleteRecursive/2" "Test" Nothing Nothing
    x1 <- putKey client put1
    context "testDeleteKey: Write failed" $ shouldBe True x1
    x2 <- putKey client put2
    context "testDeleteKey: Write failed" $ shouldBe True x2
    deleteKey client "/testDeleteRecursive/" True
    x3 <- getKey client "/testDeleteRecursive/1" Nothing Nothing
    context "testDeleteKey: Key was not deleted" $ shouldBe Nothing x3
