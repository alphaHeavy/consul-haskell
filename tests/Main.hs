{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.UUID
import Network.Consul (createSession, deleteKey, destroySession,getKey, getSequencerForLock,getSessionInfo,initializeConsulClient, isValidSequencer,putKey,putKeyAcquireLock,withSession,ConsulClient(..),runService,getServiceHealth)
import Network.Consul.Types
import qualified Network.Consul.Internal as I
import Network.HTTP.Client
import Network.Socket (PortNumber(..))
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

consulPort :: PortNumber
consulPort = 18500

newClient :: IO ConsulClient
newClient = initializeConsulClient "localhost" consulPort Nothing

{- Internal Tests -}
internalKVTests :: TestTree
internalKVTests = testGroup "Internal Key Value" [testGetInvalidKey, testPutKey, testPutKeyAcquireLock,testPutKeyReleaseLock,
  testGetKey,testGetKeys,testListKeys,testDeleteKey,testGetNullValueKey,testDeleteRecursive]

testGetInvalidKey :: TestTree
testGetInvalidKey = testCase "testGetInvalidKey" $ do
  client@ConsulClient{..} <- newClient
  x <- I.getKey ccManager (I.hostWithScheme client) ccPort "nokey" Nothing Nothing Nothing
  assertEqual "testGetInvalidKey: Found a key that doesn't exist" x Nothing

testPutKey :: TestTree
testPutKey = testCase "testPutKey" $ do
  client@ConsulClient{..} <- newClient
  let put = KeyValuePut "/testPutKey" "Test" Nothing Nothing
  x <- I.putKey ccManager (I.hostWithScheme client) ccPort put Nothing
  assertEqual "testPutKey: Write failed" True x

testPutKeyAcquireLock :: TestTree
testPutKeyAcquireLock = testCase "testPutKeyAcquireLock" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testPutKeyAcquireLock") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme client) ccPort req Nothing
  case result of
    Nothing -> assertFailure "testPutKeyAcquireLock: No session was created"
    Just session -> do
      let put = KeyValuePut "/testPutKeyAcquireLock" "Test" Nothing Nothing
      x <- I.putKeyAcquireLock ccManager (I.hostWithScheme client) ccPort put session Nothing
      assertEqual "testPutKeyAcquireLock: Write failed" True x
      Just kv <- I.getKey ccManager (I.hostWithScheme client) ccPort "/testPutKeyAcquireLock" Nothing Nothing Nothing
      let Just returnedSession = kvSession kv
      assertEqual "testPutKeyAcquireLock: Session was not found on key" returnedSession (sId session)

testPutKeyReleaseLock :: TestTree
testPutKeyReleaseLock = testCase "testPutKeyReleaseLock" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testPutKeyReleaseLock") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme client) ccPort req Nothing
  case result of
    Nothing -> assertFailure "testPutKeyReleaseLock: No session was created"
    Just session -> do
      let put = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
      x <- I.putKeyAcquireLock ccManager (I.hostWithScheme client) ccPort put session Nothing
      assertEqual "testPutKeyReleaseLock: Write failed" True x
      Just kv <- I.getKey ccManager (I.hostWithScheme client) ccPort "/testPutKeyReleaseLock" Nothing Nothing Nothing
      let Just returnedSession = kvSession kv
      assertEqual "testPutKeyReleaseLock: Session was not found on key" returnedSession (sId session)
      let put2 = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
      x2 <- I.putKeyReleaseLock ccManager (I.hostWithScheme client) ccPort put2 session Nothing
      assertEqual "testPutKeyReleaseLock: Release failed" True x2
      Just kv2 <- I.getKey ccManager (I.hostWithScheme client) ccPort "/testPutKeyReleaseLock" Nothing Nothing Nothing
      assertEqual "testPutKeyAcquireLock: Session still held" Nothing (kvSession kv2)



testGetKey :: TestTree
testGetKey = testCase "testGetKey" $ do
  client@ConsulClient{..} <- newClient
  let put = KeyValuePut "/testGetKey" "Test" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme client) ccPort put Nothing
  assertEqual "testGetKey: Write failed" True x1
  x2 <- I.getKey ccManager (I.hostWithScheme client) ccPort "/testGetKey" Nothing Nothing Nothing
  case x2 of
    Just x -> assertEqual "testGetKey: Incorrect Value" (kvValue x) (Just "Test")
    Nothing -> assertFailure "testGetKey: No value returned"

testGetNullValueKey :: TestTree
testGetNullValueKey = testCase "testGetNullValueKey" $ do
  client@ConsulClient{..} <- newClient
  let put = KeyValuePut "/testGetNullValueKey" "" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme client) ccPort put Nothing
  assertEqual "testGetNullValueKey: Write failed" True x1
  liftIO $ threadDelay (500 * 1000)
  x2 <- I.getKey ccManager (I.hostWithScheme client) ccPort "/testGetNullValueKey" Nothing Nothing Nothing
  case x2 of
    Just x -> assertEqual "testGetNullValueKey: Incorrect Value" (kvValue x) Nothing
    Nothing -> assertFailure "testGetNullValueKey: No value returned"

testGetKeys :: TestTree
testGetKeys = testCase "testGetKeys" $ do
  client@ConsulClient{..} <- newClient
  let put1 = KeyValuePut "/testGetKeys/key1" "Test" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme client) ccPort put1 Nothing
  assertEqual "testGetKeys: Write failed" True x1
  let put2 = KeyValuePut "/testGetKeys/key2" "Test" Nothing Nothing
  x2 <- I.putKey ccManager (I.hostWithScheme client) ccPort put2 Nothing
  assertEqual "testGetKeys: Write failed" True x2
  x3 <- I.getKeys ccManager (I.hostWithScheme client) ccPort "/testGetKeys" Nothing Nothing Nothing
  assertEqual "testGetKeys: Incorrect number of results" 2 (length x3)

testListKeys :: TestTree
testListKeys = testCase "testListKeys" $ do
  client@ConsulClient{..} <- newClient
  let put1 = KeyValuePut "/testListKeys/key1" "Test" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme client) ccPort put1 Nothing
  assertEqual "testListKeys: Write failed" True x1
  let put2 = KeyValuePut "/testListKeys/key2" "Test" Nothing Nothing
  x2 <- I.putKey ccManager (I.hostWithScheme client) ccPort put2 Nothing
  assertEqual "testListKeys: Write failed" True x2
  x3 <- I.listKeys ccManager (I.hostWithScheme client) ccPort "/testListKeys/" Nothing Nothing Nothing
  assertEqual "testListKeys: Incorrect number of results" 2 (length x3)

testDeleteKey :: TestTree
testDeleteKey = testCase "testDeleteKey" $ do
  client@ConsulClient{..} <- newClient
  let put1 = KeyValuePut "/testDeleteKey" "Test" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme client) ccPort put1 Nothing
  assertEqual "testDeleteKey: Write failed" True x1
  x2 <- I.deleteKey ccManager (I.hostWithScheme client) ccPort "/testDeleteKey" False Nothing
  assertEqual "testDeleteKey: Delete Failed" True x2
  x3 <- I.getKey ccManager (I.hostWithScheme client) ccPort "/testDeleteKey" Nothing Nothing Nothing
  assertEqual "testDeleteKey: Key was not deleted" Nothing x3

testDeleteRecursive :: TestTree
testDeleteRecursive = testCase "testDeleteRecursive" $ do
  client@ConsulClient{..} <- newClient
  let put1 = KeyValuePut "/testDeleteRecursive/1" "Test" Nothing Nothing
      put2 = KeyValuePut "/testDeleteRecursive/2" "Test" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme client) ccPort put1 Nothing
  assertEqual "testDeleteKey: Write failed" True x1
  x2 <- I.putKey ccManager (I.hostWithScheme client) ccPort put2 Nothing
  assertEqual "testDeleteKey: Write failed" True x2
  I.deleteKey ccManager (I.hostWithScheme client) ccPort "/testDeleteRecursive/" True Nothing
  x3 <- I.getKey ccManager (I.hostWithScheme client) ccPort "/testDeleteRecursive/1" Nothing Nothing Nothing
  assertEqual "testDeleteKey: Key was not deleted" Nothing x3

{- Client KV -}
clientKVTests :: TestTree
clientKVTests = testGroup "Client KV Tests" [testDeleteRecursiveClient]

testDeleteRecursiveClient :: TestTree
testDeleteRecursiveClient = testCase "testDeleteRecursiveClient" $ do
  client <- newClient
  let put1 = KeyValuePut "/testDeleteRecursive/1" "Test" Nothing Nothing
      put2 = KeyValuePut "/testDeleteRecursive/2" "Test" Nothing Nothing
  x1 <- putKey client put1 Nothing
  assertEqual "testDeleteKey: Write failed" True x1
  x2 <- putKey client put2 Nothing
  assertEqual "testDeleteKey: Write failed" True x2
  deleteKey client "/testDeleteRecursive/" True Nothing
  x3 <- getKey client "/testDeleteRecursive/1" Nothing Nothing Nothing
  assertEqual "testDeleteKey: Key was not deleted" Nothing x3

{- Agent -}
testRegisterService :: TestTree
testRegisterService = testCase "testRegisterService" $ do
  client@ConsulClient{..} <- newClient
  let req = RegisterService Nothing "testService" ["test"] Nothing (Just $ Ttl "10s")
  val <- I.registerService ccManager (I.hostWithScheme client) ccPort req Nothing
  assertEqual "testRegisterService: Service was not created" val True
  mService <- I.getService ccManager (I.hostWithScheme client) ccPort "testService" Nothing Nothing
  case mService of
    Just _ -> return ()
    Nothing -> assertFailure "testRegisterService: Service was not found"

testGetSelf :: TestTree
testGetSelf = testCase "testGetSelf" $ do
  client@ConsulClient{..} <- newClient
  x <- I.getSelf ccManager (I.hostWithScheme client) ccPort
  assertEqual "testGetSelf: Self not returned" True (isJust x)

{-
testRegisterHealthCheck :: TestTree
testRegisterHealthCheck = testCase "testRegisterHealthCheck" $ do
  client@ConsulClient{..} <- newClient
  let check = RegisterHealthCheck "testHealthCheck" "testHealthCheck" "" Nothing Nothing (Just "15s")
  x1 <- I.registerHealthCheck ccManager (I.hostWithScheme client) ccPort check
  undefined -}

{- Health Checks -}
testGetServiceHealth :: TestTree
testGetServiceHealth = testCase "testGetServiceHealth" $ do
  client@ConsulClient{..} <- newClient
  let req = RegisterService (Just "testGetServiceHealth") "testGetServiceHealth" [] Nothing Nothing
  r1 <- I.registerService ccManager (I.hostWithScheme client) ccPort req Nothing
  case r1 of
    True -> do
      liftIO $ threadDelay 1000000
      r2 <- I.getServiceHealth ccManager (I.hostWithScheme client) ccPort "testGetServiceHealth"
      case r2 of
        Just [x] -> return ()
        Just [] -> assertFailure "testGetServiceHealth: No Services Returned"
        Nothing -> assertFailure "testGetServiceHealth: Failed to parse result"
    False -> assertFailure "testGetServiceHealth: Service was not created"

testHealth :: TestTree
testHealth = testGroup "Health Check Tests" [testGetServiceHealth]

{- Session -}
testCreateSession :: TestTree
testCreateSession = testCase "testCreateSession" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testCreateSession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme client) ccPort req Nothing
  case result of
    Just _ -> return ()
    Nothing -> assertFailure "testCreateSession: No session was created"

testGetSessionInfo :: TestTree
testGetSessionInfo = testCase "testGetSessionInfo" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testGetSessionInfo") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme client) ccPort req Nothing
  case result of
    Just x -> do
      x1 <- I.getSessionInfo ccManager (I.hostWithScheme client) ccPort x Nothing
      case x1 of
        Just _ -> return ()
        Nothing -> assertFailure "testGetSessionInfo: Session Info was not returned"
    Nothing -> assertFailure "testGetSessionInfo: No session was created"

testRenewSession :: TestTree
testRenewSession = testCase "testRenewSession" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testRenewSession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme client) ccPort req Nothing
  case result of
    Just x -> do
      x1 <- I.renewSession ccManager (I.hostWithScheme client) ccPort x Nothing
      case x1 of
        True -> return ()
        False -> assertFailure "testRenewSession: Session was not renewed"
    Nothing -> assertFailure "testRenewSession: No session was created"

testRenewNonexistentSession :: TestTree
testRenewNonexistentSession = testCase "testRenewNonexistentSession" $ do
  client@ConsulClient{..} <- newClient
  sessId :: UUID <- randomIO
  let session = Session (toText sessId) Nothing
  x <- I.renewSession ccManager (I.hostWithScheme client) ccPort session Nothing
  case x of
    True -> assertFailure "testRenewNonexistentSession: Non-existent session was renewed"
    False -> return ()

testDestroySession :: TestTree
testDestroySession = testCase "testDestroySession" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testDestroySession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme client) ccPort req Nothing
  case result of
    Just x -> do
      _ <- I.destroySession ccManager (I.hostWithScheme client) ccPort x Nothing
      x1 <- I.getSessionInfo ccManager (I.hostWithScheme client) ccPort x Nothing
      assertBool "testDestroySession: Session info was returned after destruction" $ (x1 == Nothing) || (x1 == Just [])
    Nothing -> assertFailure "testDestroySession: No session was created"

testInternalSession :: TestTree
testInternalSession = testGroup "Internal Session Tests" [testCreateSession, testGetSessionInfo, testRenewSession, testRenewNonexistentSession, testDestroySession]


testSessionMaintained :: TestTree
testSessionMaintained = testCase "testSessionMaintained" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testSessionMaintained") Nothing ["serfHealth"] (Just Release) (Just "10s")
  result <- I.createSession ccManager (I.hostWithScheme client) ccPort req Nothing
  case result of
    Just session -> do
      threadDelay (12 * 1000000)
      y <- getSessionInfo client session Nothing
      assertEqual "testSessionMaintained: Session not found" True (isJust y)
    Nothing -> assertFailure "testSessionMaintained: No Session was created"


testWithSessionCancel :: TestTree
testWithSessionCancel = testCase "testWithSessionCancel" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testWithSessionCancel") Nothing ["serfHealth"] (Just Release) (Just "10s")
  result <- I.createSession ccManager (I.hostWithScheme client) ccPort req Nothing
  case result of
    Just session -> do
      x1 <- withSession client Nothing 5 session (\ y -> action y client ) cancelAction
      assertEqual "testWithSessionCancel: Incorrect value" "Canceled" x1
      z <- getSessionInfo client session Nothing
      assertBool "testWithSessionCancel: Session was found" $ (z == Nothing) || (z == Just [])
    Nothing -> assertFailure "testWithSessionCancel: No session was created"
  where
    action :: MonadIO m => Session -> ConsulClient -> m Text
    action x client@ConsulClient{..} = do
      I.destroySession ccManager (I.hostWithScheme client) ccPort x Nothing
      liftIO $ threadDelay (30 * 1000000)
      return ("NotCanceled" :: Text)

    cancelAction :: MonadIO m => m Text
    cancelAction = return ("Canceled" :: Text)


testRunServiceTtl :: TestTree
testRunServiceTtl = testCase "testRunServiceTtl" $ do
  client@ConsulClient{..} <- newClient
  let register = RegisterService Nothing "testRunServiceTtl" [] (Just 8000) $ Just $ Ttl "10s"
  runService client register (action client) Nothing
  where
    action client = do
      threadDelay 15000000
      mHealth <- getServiceHealth client "testRunServiceTtl"
      case mHealth of
        Nothing -> assertFailure "testRunServiceTtl: No healthcheck was found"
        Just [x] -> do
          let checks = hChecks x
          mapM_ (testCheck) checks
    testCheck check = do
      assertBool "testRunServiceTtl: Check not passing" $ cStatus check == Passing


{-testSequencerLostSession :: TestTree
testSequencerLostSession = testCase "testSequencerLostSession" $ do
  client@ConsulClient{..} <- initializeConsulClient "localhost" consulPort Nothing
-}

testIsValidSequencer :: TestTree
testIsValidSequencer = testCase "testIsValidSequencer" $ do
  client@ConsulClient{..} <- initializeConsulClient "localhost" consulPort Nothing
  let req = SessionRequest Nothing (Just "testIsValidSequencer") Nothing ["serfHealth"] (Just Release) (Just "10s")
  result <- createSession client req Nothing
  case result of
    Nothing -> assertFailure "testIsValidSequencer: No session was created"
    Just session -> do
      let put = KeyValuePut "/testIsValidSequencer" "Test" Nothing Nothing
      x <- putKeyAcquireLock client put session Nothing
      assertEqual "testIsValidSequencer: Write failed" True x
      Just sequencer <- getSequencerForLock client "/testIsValidSequencer" session Nothing
      result1 <- isValidSequencer client sequencer Nothing
      assertEqual "testIsValidSequencer: Valid sequencer was invalid" True result1
      _ <- destroySession client session Nothing
      result2 <- isValidSequencer client sequencer Nothing
      assertEqual "testIsValidSequencer: Invalid session was valid" False result2


sessionWorkflowTests :: TestTree
sessionWorkflowTests = testGroup "Session Workflow Tests" [testWithSessionCancel,testSessionMaintained]

runServiceTests :: TestTree
runServiceTests = testGroup "Run Service Tests" [testRunServiceTtl]

agentTests :: TestTree
agentTests = testGroup "Agent Tests" [testGetSelf,testRegisterService]

sequencerTests :: TestTree
sequencerTests = testGroup "Sequencer Tests" [testIsValidSequencer]

allTests :: TestTree
allTests = testGroup "All Tests" [testInternalSession, internalKVTests, sessionWorkflowTests, agentTests,testHealth, clientKVTests, runServiceTests, sequencerTests]

main :: IO ()
main = defaultMain allTests
