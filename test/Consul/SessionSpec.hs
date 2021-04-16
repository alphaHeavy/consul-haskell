{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Consul.SessionSpec (spec) where

import Import
import Test.Syd

spec :: Spec
spec = do
  pure ()

{- Session -}
-- testCreateSession :: TestTree
-- testCreateSession = testCase "testCreateSession" $ do
--   client@ConsulClient{..} <- newClient
--   let ttl = "30s"
--       req =
--         SessionRequest
--           lockDelay
--           (Just "testCreateSession")
--           localNode
--           checkIds
--           (Just Release)
--           (Just ttl)
--   let loopUntilSession :: IO ()
--       loopUntilSession = do
--         resp <- createSession client req
--         case resp of
--           Just _ -> return ()
--           Nothing -> do
--             putStrLn "Session creation failed, retrying..."
--             sleep 0.05  -- pause for 50ms
--             loopUntilSession
--   result <- timeout fiveSecondMicros loopUntilSession
--   case result of
--     Just _ -> return ()
--     Nothing -> assertFailure $ "testCreateSession: Session creation failed after retrying for 5 seconds"
-- 
-- 
-- testGetSessionInfo :: TestTree
-- testGetSessionInfo = testCase "testGetSessionInfo" $ do
--   client@ConsulClient{..} <- newClient
--   let ttl = "30s"
--       req =
--         SessionRequest
--           lockDelay
--           (Just "testGetSessionInfo")
--           localNode
--           checkIds
--           (Just Release)
--           (Just ttl)
--   result <- createSession client req
--   case result of
--     Just x -> do
--       sleep 1
--       x1 <- getSessionInfo client x
--       case x1 of
--         Just _ -> return ()
--         Nothing -> assertFailure "testGetSessionInfo: Session Info was not returned"
--     Nothing -> assertFailure "testGetSessionInfo: No session was created"
-- 
-- testRenewSession :: TestTree
-- testRenewSession = testCase "testRenewSession" $ do
--   client@ConsulClient{..} <- newClient
--   let ttl = "30s"
--       req = SessionRequest Nothing (Just "testRenewSession") localNode checkIds (Just Release) (Just ttl)
--   result <- createSession client req
--   case result of
--     Just x -> do
--       x1 <- renewSession client x
--       case x1 of
--         True -> return ()
--         False -> assertFailure "testRenewSession: Session was not renewed"
--     Nothing -> assertFailure "testRenewSession: No session was created"
-- 
-- testRenewNonexistentSession :: TestTree
-- testRenewNonexistentSession = testCase "testRenewNonexistentSession" $ do
--   client@ConsulClient{..} <- newClient
--   sessId :: UUID <- randomIO
--   let session = Session (toText sessId) Nothing
--   x <- renewSession client session
--   case x of
--     True -> assertFailure "testRenewNonexistentSession: Non-existent session was renewed"
--     False -> return ()
-- 
-- testDestroySession :: TestTree
-- testDestroySession = testCase "testDestroySession" $ do
--   client@ConsulClient{..} <- newClient
--   let ttl = "30s"
--       req = SessionRequest Nothing (Just "testDestroySession") localNode checkIds (Just Release) (Just ttl)
--   result <- createSession client req
--   case result of
--     Just x -> do
--       _ <- destroySession client x
--       x1 <- getSessionInfo client x
--       assertBool "testDestroySession: Session info was returned after destruction" $ (x1 == Nothing) || (x1 == Just [])
--     Nothing -> assertFailure "testDestroySession: No session was created"
-- 
-- testInternalSession :: TestTree
-- testInternalSession = testGroup "Internal Session Tests" [testCreateSession, testGetSessionInfo, testRenewSession, testRenewNonexistentSession, testDestroySession]
-- 
-- testSessionMaintained :: TestTree
-- testSessionMaintained = testCase "testSessionMaintained" $ do
--   client@ConsulClient{..} <- newClient
--   let req = SessionRequest Nothing (Just "testSessionMaintained") localNode checkIds (Just Release) (Just "15s")
--   result <- createSession client req
--   case result of
--     Just session -> do
--       sleep 12
--       y <- getSessionInfo client session
--       assertEqual "testSessionMaintained: Session not found" True (isJust y)
--     Nothing -> assertFailure "testSessionMaintained: No Session was created"
-- 
-- 
-- testWithSessionCancel :: TestTree
-- testWithSessionCancel = testCase "testWithSessionCancel" $ do
--   client@ConsulClient{..} <- newClient
--   let req = SessionRequest Nothing (Just "testWithSessionCancel") localNode checkIds (Just Release) (Just "10s")
--   result <- createSession client req
--   case result of
--     Just session -> do
--       x1 <- withSession client Nothing 5 session (\ y -> action y client ) cancelAction
--       assertEqual "testWithSessionCancel: Incorrect value" "Canceled" x1
--       z <- getSessionInfo client session
--       assertBool "testWithSessionCancel: Session was found" $ (z == Nothing) || (z == Just [])
--     Nothing -> assertFailure "testWithSessionCancel: No session was created"
--   where
--     action :: MonadIO m => Session -> ConsulClient -> m Text
--     action x client@ConsulClient{..} = do
--       destroySession client x
--       liftIO $ sleep 30
--       return ("NotCanceled" :: Text)
-- 
--     cancelAction :: MonadIO m => m Text
--     cancelAction = return ("Canceled" :: Text)
-- 
-- 
-- {-testSequencerLostSession :: TestTree
-- testSequencerLostSession = testCase "testSequencerLostSession" $ do
--   client@ConsulClient{..} <- initializeConsulClient "localhost" consulPort Nothing
-- -}
-- 
-- -- TODO: drop stringified values (localhost, dc1, etc)
-- testIsValidSequencer :: TestTree
-- testIsValidSequencer = testCase "testIsValidSequencer" $ do
--   client@ConsulClient{..} <- initializeConsulClient localhost consulPort Nothing
--   let req = SessionRequest Nothing (Just "testIsValidSequencer") localNode checkIds (Just Release) (Just "10s")
--   result <- createSession client req
--   case result of
--     Nothing -> assertFailure "testIsValidSequencer: No session was created"
--     Just session -> do
--       let put = KeyValuePut "/testIsValidSequencer" "Test" Nothing Nothing
--       x <- putKeyAcquireLock client put session
--       assertEqual "testIsValidSequencer: Write failed" True x
--       Just sequencer <- getSequencerForLock client "/testIsValidSequencer" session
--       result1 <- isValidSequencer client sequencer
--       assertEqual "testIsValidSequencer: Valid sequencer was invalid" True result1
--       _ <- destroySession client session
--       result2 <- isValidSequencer client sequencer
--       assertEqual "testIsValidSequencer: Invalid session was valid" False result2
-- 
