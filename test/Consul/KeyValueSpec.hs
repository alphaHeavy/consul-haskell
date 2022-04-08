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
spec = aroundAll withConsulServer $ do

  itWithOuter "Get Invalid Key" $ \_ -> (do
    client@ConsulClient{..} <- newClient
    -- specify the datacenter as part of our request
    x <- getKey client{ ccDatacenter = dc1  } "nokey" Nothing Nothing
    shouldBe "testGetInvalidKey: Found a key that doesn't exist" x Nothing)

  itWithOuter "testPutKey" $ \_ -> (do
    client@ConsulClient{..} <- newClient
    let put = KeyValuePut "/testPutKey" "Test" Nothing Nothing
    x <- putKey client put
    shouldBe "testPutKey: Write failed" True x)

  itWithOuter "testPutKeyAcquireLock" $ \_ -> (do
    client@ConsulClient{..} <- newClient
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
      Nothing -> assertFailure "testPutKeyAcquireLock: No session was created"
      Just session -> do
        let put = KeyValuePut "/testPutKeyAcquireLock" "Test" Nothing Nothing
        x <- putKeyAcquireLock client put session
        shouldBe "testPutKeyAcquireLock: Write failed" True x
        Just kv <- getKey client "/testPutKeyAcquireLock" Nothing Nothing
        let Just returnedSession = kvSession kv
        shouldBe "testPutKeyAcquireLock: Session was not found on key" returnedSession (sId session))


  itWithOuter "testPutKeyReleaseLock" $ \_ -> (do
    client@ConsulClient{..} <- newClient
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
      Nothing -> assertFailure "testPutKeyReleaseLock: No session was created"
      Just session -> do
        let put = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
        x <- putKeyAcquireLock client put session
        shouldBe "testPutKeyReleaseLock: Write failed" True x
        Just kv <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
        let Just returnedSession = kvSession kv
        shouldBe "testPutKeyReleaseLock: Session was not found on key" returnedSession (sId session)
        let put2 = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
        x2 <- putKeyReleaseLock client put2 session
        shouldBe "testPutKeyReleaseLock: Release failed" True x2
        Just kv2 <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
        shouldBe "testPutKeyAcquireLock: Session still held" Nothing (kvSession kv2))


  itWithOuter "testGetKey" $ \_ -> (do
    client@ConsulClient{..} <- newClient
    let put = KeyValuePut "/testGetKey" "Test" Nothing Nothing
    x1 <- putKey client put
    shouldBe "testGetKey: Write failed" True x1
    x2 <- getKey client "/testGetKey" Nothing Nothing
    case x2 of
      Just x -> shouldBe "testGetKey: Incorrect Value" (kvValue x) (Just "Test")
      Nothing -> assertFailure "testGetKey: No value returned")

  itWithOuter "testGetNullValueKey" $ \_ -> (do
    client@ConsulClient{..} <- newClient
    let put = KeyValuePut "/testGetNullValueKey" "" Nothing Nothing
    x1 <- putKey client put
    shouldBe "testGetNullValueKey: Write failed" True x1
    liftIO $ sleep 0.5
    x2 <- getKey client "/testGetNullValueKey" Nothing Nothing
    case x2 of
      Just x -> shouldBe "testGetNullValueKey: Incorrect Value" (kvValue x) Nothing
      Nothing -> assertFailure "testGetNullValueKey: No value returned")

  itWithOuter "testGetKeys" $ \_ -> (do
    client@ConsulClient{..} <- newClient
    let put1 = KeyValuePut "/testGetKeys/key1" "Test" Nothing Nothing
    x1 <- putKey client put1
    shouldBe "testGetKeys: Write failed" True x1
    let put2 = KeyValuePut "/testGetKeys/key2" "Test" Nothing Nothing
    x2 <- putKey client put2
    shouldBe "testGetKeys: Write failed" True x2
    x3 <- getKeys client "/testGetKeys" Nothing Nothing
    shouldBe "testGetKeys: Incorrect number of results" 2 (length x3))

  itWithOuter "testListKeys" $ \_ -> (do
    client@ConsulClient{..} <- newClient
    let put1 = KeyValuePut "/testListKeys/key1" "Test" Nothing Nothing
    x1 <- putKey client put1
    shouldBe "testListKeys: Write failed" True x1
    let put2 = KeyValuePut "/testListKeys/key2" "Test" Nothing Nothing
    x2 <- putKey client put2
    shouldBe "testListKeys: Write failed" True x2
    x3 <- listKeys client "/testListKeys/" Nothing Nothing
    shouldBe "testListKeys: Incorrect number of results" 2 (length x3))

  itWithOuter "testDeleteKey" $ \_ -> (do
    client@ConsulClient{..} <- newClient
    let put1 = KeyValuePut "/testDeleteKey" "Test" Nothing Nothing
    x1 <- putKey client put1
    shouldBe "testDeleteKey: Write failed" True x1
    x2 <- deleteKey client "/testDeleteKey" False
    shouldBe "testDeleteKey: Delete Failed" True x2
    x3 <- getKey client "/testDeleteKey" Nothing Nothing
    shouldBe "testDeleteKey: Key was not deleted" Nothing x3)

  itWithOuter "testDeleteRecursive" $ \_ -> (do
    client@ConsulClient{..} <- newClient
    let put1 = KeyValuePut "/testDeleteRecursive/1" "Test" Nothing Nothing
        put2 = KeyValuePut "/testDeleteRecursive/2" "Test" Nothing Nothing
    x1 <- putKey client put1
    shouldBe "testDeleteKey: Write failed" True x1
    x2 <- putKey client put2
    shouldBe "testDeleteKey: Write failed" True x2
    deleteKey client "/testDeleteRecursive/" True
    x3 <- getKey client "/testDeleteRecursive/1" Nothing Nothing
    shouldBe "testDeleteKey: Key was not deleted" Nothing x3)
