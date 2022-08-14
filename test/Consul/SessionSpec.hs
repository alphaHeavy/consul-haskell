{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Consul.SessionSpec (spec) where

import Import

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.UUID (UUID, toText)
import System.Random (randomIO)
import System.Timeout (timeout)
import Test.Syd

spec :: Spec
spec = setupAround consulServerSetupFunc $ modifyMaxSuccess (`div` 20) $ do

  it "testCreateSession" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let nodeName = consulServerHandleNodeName consulServerHandle
        ttl = "30s"
        req =
          SessionRequest
            lockDelay
            (Just "testCreateSession")
            nodeName
            checkIds
            (Just Release)
            (Just ttl)
    let loopUntilSession :: IO ()
        loopUntilSession = do
          resp <- createSession client req
          case resp of
            Just _ -> return ()
            Nothing -> do
              putStrLn "Session creation failed, retrying..."
              sleep 0.10  -- pause for 50ms
              loopUntilSession
    result <- timeout fiveSecondMicros loopUntilSession
    case result of
      Just _ -> return ()
      Nothing -> expectationFailure $ "testCreateSession: Session creation failed after retrying for 5 seconds"

  it "testGetSessionInfo" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let nodeName = consulServerHandleNodeName consulServerHandle
        ttl = "30s"
        req =
          SessionRequest
            lockDelay
            (Just "testGetSessionInfo")
            nodeName
            checkIds
            (Just Release)
            (Just ttl)
    result <- createSession client req
    case result of
      Just x -> do
        sleep 1
        x1 <- getSessionInfo client x
        case x1 of
          Just _ -> return ()
          Nothing -> expectationFailure "testGetSessionInfo: Session Info was not returned"
      Nothing -> expectationFailure "testGetSessionInfo: No session was created"
   
  it "testRenewSession" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let nodeName = consulServerHandleNodeName consulServerHandle
        ttl = "30s"
        req =
          SessionRequest
            Nothing
            (Just "testRenewSession")
            nodeName
            checkIds
            (Just Release)
            (Just ttl)
    result <- createSession client req
    case result of
      Just x -> do
        x1 <- renewSession client x
        case x1 of
          True -> return ()
          False -> expectationFailure "testRenewSession: Session was not renewed"
      Nothing -> expectationFailure "testRenewSession: No session was created"

  it "testRenewNonexistentSession" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    sessId :: UUID <- randomIO
    let session = Session (toText sessId) Nothing
    x <- renewSession client session
    case x of
      True -> expectationFailure "testRenewNonexistentSession: Non-existent session was renewed"
      False -> return ()

  it "testDestroySession" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let nodeName = consulServerHandleNodeName consulServerHandle
        ttl = "30s"
        req =
          SessionRequest
            Nothing
            (Just "testDestroySession")
            nodeName
            checkIds
            (Just Release)
            (Just ttl)
    result <- createSession client req
    case result of
      Just x -> do
        _ <- destroySession client x
        x1 <- getSessionInfo client x
        context "testDestroySession: Session info was returned after destruction" $ (isNothing x1) || (x1 == Just []) `shouldBe` (True || False)
      Nothing -> expectationFailure "testDestroySession: No session was created"

  it "testSessionMaintained" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let nodeName = consulServerHandleNodeName consulServerHandle
        req =
          SessionRequest
            Nothing
            (Just "testSessionMaintained")
            nodeName
            checkIds
            (Just Release)
            (Just "15s")
    result <- createSession client req
    case result of
      Just session -> do
        sleep 12
        y <- getSessionInfo client session
        context "testSessionMaintained: Session not found" $ (isJust y) `shouldBe` True
      Nothing -> expectationFailure "testSessionMaintained: No Session was created"

  it "testWithSessionCancel" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    let nodeName = consulServerHandleNodeName consulServerHandle
        req =
          SessionRequest
            Nothing
            (Just "testWithSessionCancel")
            nodeName
            checkIds
            (Just Release)
            (Just "10s")
    result <- createSession client req
    case result of
      Just session -> do
        x1 <- withSession client Nothing 5 session (\ y -> action y client ) cancelAction
        context "testWithSessionCancel: Incorrect value" $ x1 `shouldBe` "Canceled"
        z <- getSessionInfo client session
        context "testWithSessionCancel: Session was found" $ ((isNothing z) || (z == Just [])) `shouldBe` (True || False)
      Nothing -> expectationFailure "testWithSessionCancel: No session was created"
    where
      action :: MonadIO m => Session -> ConsulClient -> m Text
      action x client@ConsulClient{..} = do
        destroySession client x
        liftIO $ sleep 30
        return ("NotCanceled" :: Text)

      cancelAction :: MonadIO m => m Text
      cancelAction = return ("Canceled" :: Text)


{-testSequencerLostSession :: TestTree
testSequencerLostSession = testCase "testSequencerLostSession" $ do
  client@ConsulClient{..} <- initializeConsulClient "localhost" consulPort Nothing
-}

-- TODO: drop stringified values (localhost, dc1, etc)
--  it "testIsValidSequencer" \consulServerHandle -> $ do
--    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
--    let req = SessionRequest Nothing (Just "testIsValidSequencer") localNode checkIds (Just Release) (Just "10s")
--    result <- createSession client req
--    case result of
--      Nothing -> expectationFailure "testIsValidSequencer: No session was created"
--      Just session -> do
--        let put = KeyValuePut "/testIsValidSequencer" "Test" Nothing Nothing
--        x <- putKeyAcquireLock client put session
--        assertEqual "testIsValidSequencer: Write failed" True x
--        Just sequencer <- getSequencerForLock client "/testIsValidSequencer" session
--        result1 <- isValidSequencer client sequencer
--        assertEqual "testIsValidSequencer: Valid sequencer was invalid" True result1
--        _ <- destroySession client session
--        result2 <- isValidSequencer client sequencer
--        assertEqual "testIsValidSequencer: Invalid session was valid" False result2

