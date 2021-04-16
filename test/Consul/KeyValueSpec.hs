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
  itWithOuter "does not crash" $ \_ -> (do sleep 15 :: IO ())

  itWithOuter "Get Invalid Key" $ \_ -> (do
    client@ConsulClient{..} <- newClient
    -- specify the datacenter as part of our request
    x <- getKey client{ ccDatacenter = dc1  } "nokey" Nothing Nothing
    assertEqual "testGetInvalidKey: Found a key that doesn't exist" x Nothing)

-- testPutKey :: TestTree
-- testPutKey = testCase "testPutKey" $ do
--   client@ConsulClient{..} <- newClient
--   let put = KeyValuePut "/testPutKey" "Test" Nothing Nothing
--   x <- putKey client put
--   assertEqual "testPutKey: Write failed" True x
-- 
-- testPutKeyAcquireLock :: TestTree
-- testPutKeyAcquireLock = testCase "testPutKeyAcquireLock" $ do
--   client@ConsulClient{..} <- newClient
--   let ttl = "30s"
--       req =
--         SessionRequest
--           lockDelay
--           (Just "testPutKeyAcquireLock")
--           localNode
--           checkIds
--           (Just Release)
--           (Just ttl)
--   result <- createSession client req
--   case result of
--     Nothing -> assertFailure "testPutKeyAcquireLock: No session was created"
--     Just session -> do
--       let put = KeyValuePut "/testPutKeyAcquireLock" "Test" Nothing Nothing
--       x <- putKeyAcquireLock client put session
--       assertEqual "testPutKeyAcquireLock: Write failed" True x
--       Just kv <- getKey client "/testPutKeyAcquireLock" Nothing Nothing
--       let Just returnedSession = kvSession kv
--       assertEqual "testPutKeyAcquireLock: Session was not found on key" returnedSession (sId session)
-- 
-- testPutKeyReleaseLock :: TestTree
-- testPutKeyReleaseLock = testCase "testPutKeyReleaseLock" $ do
--   client@ConsulClient{..} <- newClient
--   let ttl = "30s"
--       req =
--         SessionRequest
--           Nothing
--           (Just "testPutKeyReleaseLock")
--           localNode
--           checkIds
--           (Just Release)
--           (Just ttl)
--   result <- createSession client req
--   case result of
--     Nothing -> assertFailure "testPutKeyReleaseLock: No session was created"
--     Just session -> do
--       let put = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
--       x <- putKeyAcquireLock client put session
--       assertEqual "testPutKeyReleaseLock: Write failed" True x
--       Just kv <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
--       let Just returnedSession = kvSession kv
--       assertEqual "testPutKeyReleaseLock: Session was not found on key" returnedSession (sId session)
--       let put2 = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
--       x2 <- putKeyReleaseLock client put2 session
--       assertEqual "testPutKeyReleaseLock: Release failed" True x2
--       Just kv2 <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
--       assertEqual "testPutKeyAcquireLock: Session still held" Nothing (kvSession kv2)
-- 
-- 
-- testGetKey :: TestTree
-- testGetKey = testCase "testGetKey" $ do
--   client@ConsulClient{..} <- newClient
--   let put = KeyValuePut "/testGetKey" "Test" Nothing Nothing
--   x1 <- putKey client put
--   assertEqual "testGetKey: Write failed" True x1
--   x2 <- getKey client "/testGetKey" Nothing Nothing
--   case x2 of
--     Just x -> assertEqual "testGetKey: Incorrect Value" (kvValue x) (Just "Test")
--     Nothing -> assertFailure "testGetKey: No value returned"
-- 
-- testGetNullValueKey :: TestTree
-- testGetNullValueKey = testCase "testGetNullValueKey" $ do
--   client@ConsulClient{..} <- newClient
--   let put = KeyValuePut "/testGetNullValueKey" "" Nothing Nothing
--   x1 <- putKey client put
--   assertEqual "testGetNullValueKey: Write failed" True x1
--   liftIO $ sleep 0.5
--   x2 <- getKey client "/testGetNullValueKey" Nothing Nothing
--   case x2 of
--     Just x -> assertEqual "testGetNullValueKey: Incorrect Value" (kvValue x) Nothing
--     Nothing -> assertFailure "testGetNullValueKey: No value returned"
-- 
-- testGetKeys :: TestTree
-- testGetKeys = testCase "testGetKeys" $ do
--   client@ConsulClient{..} <- newClient
--   let put1 = KeyValuePut "/testGetKeys/key1" "Test" Nothing Nothing
--   x1 <- putKey client put1
--   assertEqual "testGetKeys: Write failed" True x1
--   let put2 = KeyValuePut "/testGetKeys/key2" "Test" Nothing Nothing
--   x2 <- putKey client put2
--   assertEqual "testGetKeys: Write failed" True x2
--   x3 <- getKeys client "/testGetKeys" Nothing Nothing
--   assertEqual "testGetKeys: Incorrect number of results" 2 (length x3)
-- 
-- testListKeys :: TestTree
-- testListKeys = testCase "testListKeys" $ do
--   client@ConsulClient{..} <- newClient
--   let put1 = KeyValuePut "/testListKeys/key1" "Test" Nothing Nothing
--   x1 <- putKey client put1
--   assertEqual "testListKeys: Write failed" True x1
--   let put2 = KeyValuePut "/testListKeys/key2" "Test" Nothing Nothing
--   x2 <- putKey client put2
--   assertEqual "testListKeys: Write failed" True x2
--   x3 <- listKeys client "/testListKeys/" Nothing Nothing
--   assertEqual "testListKeys: Incorrect number of results" 2 (length x3)
-- 
-- testDeleteKey :: TestTree
-- testDeleteKey = testCase "testDeleteKey" $ do
--   client@ConsulClient{..} <- newClient
--   let put1 = KeyValuePut "/testDeleteKey" "Test" Nothing Nothing
--   x1 <- putKey client put1
--   assertEqual "testDeleteKey: Write failed" True x1
--   x2 <- deleteKey client "/testDeleteKey" False
--   assertEqual "testDeleteKey: Delete Failed" True x2
--   x3 <- getKey client "/testDeleteKey" Nothing Nothing
--   assertEqual "testDeleteKey: Key was not deleted" Nothing x3
-- 
-- testDeleteRecursive :: TestTree
-- testDeleteRecursive = testCase "testDeleteRecursive" $ do
--   client@ConsulClient{..} <- newClient
--   let put1 = KeyValuePut "/testDeleteRecursive/1" "Test" Nothing Nothing
--       put2 = KeyValuePut "/testDeleteRecursive/2" "Test" Nothing Nothing
--   x1 <- putKey client put1
--   assertEqual "testDeleteKey: Write failed" True x1
--   x2 <- putKey client put2
--   assertEqual "testDeleteKey: Write failed" True x2
--   deleteKey client "/testDeleteRecursive/" True
--   x3 <- getKey client "/testDeleteRecursive/1" Nothing Nothing
--   assertEqual "testDeleteKey: Key was not deleted" Nothing x3

