{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Network.Consul (createManagedSession,getSessionInfo,initializeConsulClient,withSession,ConsulClient(..),ManagedSession(..))
import Network.Consul.Types
import qualified Network.Consul.Internal as I
import Network.HTTP.Client
import Network.Socket (PortNumber(..))
import Test.Tasty
import Test.Tasty.HUnit

client :: IO ConsulClient
client = initializeConsulClient "localhost" 8500 Nothing

{- Internal Tests -}
internalKVTests :: TestTree
internalKVTests = testGroup "Internal Key Value" [testGetInvalidKey, testPutKey,
  testGetKey,testGetKeys,testListKeys,testDeleteKey,testGetNullValueKey]

testGetInvalidKey :: TestTree
testGetInvalidKey = testCase "testGetInvalidKey" $ do
  _client@ConsulClient{..} <- client
  x <- I.getKey ccManager (I.hostWithScheme _client) ccPort "nokey" Nothing Nothing Nothing
  assertEqual "testGetInvalidKey: Found a key that doesn't exist" x Nothing

testPutKey :: TestTree
testPutKey = testCase "testPutKey" $ do
  _client@ConsulClient{..} <- client
  let put = KeyValuePut "/testPutKey" "Test" Nothing Nothing
  x <- I.putKey ccManager (I.hostWithScheme _client) ccPort put Nothing
  assertEqual "testPutKey: Write failed" True x

testGetKey :: TestTree
testGetKey = testCase "testGetKey" $ do
  _client@ConsulClient{..} <- client
  let put = KeyValuePut "/testGetKey" "Test" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme _client) ccPort put Nothing
  assertEqual "testGetKey: Write failed" True x1
  x2 <- I.getKey ccManager (I.hostWithScheme _client) ccPort "/testGetKey" Nothing Nothing Nothing
  case x2 of
    Just x -> assertEqual "testGetKey: Incorrect Value" (kvValue x) (Just "Test")
    Nothing -> assertFailure "testGetKey: No value returned"

testGetNullValueKey :: TestTree
testGetNullValueKey = testCase "testGetNullValueKey" $ do
  _client@ConsulClient{..} <- client
  let put = KeyValuePut "/testGetNullValueKey" "" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme _client) ccPort put Nothing
  assertEqual "testGetNullValueKey: Write failed" True x1
  liftIO $ threadDelay (500 * 1000)
  x2 <- I.getKey ccManager (I.hostWithScheme _client) ccPort "/testGetNullValueKey" Nothing Nothing Nothing
  case x2 of
    Just x -> assertEqual "testGetNullValueKey: Incorrect Value" (kvValue x) Nothing
    Nothing -> assertFailure "testGetNullValueKey: No value returned"


testGetKeys :: TestTree
testGetKeys = testCase "testGetKeys" $ do
  _client@ConsulClient{..} <- client
  let put1 = KeyValuePut "/testGetKeys/key1" "Test" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme _client) ccPort put1 Nothing
  assertEqual "testGetKeys: Write failed" True x1
  let put2 = KeyValuePut "/testGetKeys/key2" "Test" Nothing Nothing
  x2 <- I.putKey ccManager (I.hostWithScheme _client) ccPort put2 Nothing
  assertEqual "testGetKeys: Write failed" True x2
  x3 <- I.getKeys ccManager (I.hostWithScheme _client) ccPort "/testGetKeys" Nothing Nothing Nothing
  assertEqual "testGetKeys: Incorrect number of results" 2 (length x3)

testListKeys :: TestTree
testListKeys = testCase "testListKeys" $ do
  _client@ConsulClient{..} <- client
  let put1 = KeyValuePut "/testListKeys/key1" "Test" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme _client) ccPort put1 Nothing
  assertEqual "testListKeys: Write failed" True x1
  let put2 = KeyValuePut "/testListKeys/key2" "Test" Nothing Nothing
  x2 <- I.putKey ccManager (I.hostWithScheme _client) ccPort put2 Nothing
  assertEqual "testListKeys: Write failed" True x2
  x3 <- I.listKeys ccManager (I.hostWithScheme _client) ccPort "/testListKeys/" Nothing Nothing Nothing
  assertEqual "testListKeys: Incorrect number of results" 2 (length x3)

testDeleteKey :: TestTree
testDeleteKey = testCase "testDeleteKey" $ do
  _client@ConsulClient{..} <- client
  let put1 = KeyValuePut "/testDeleteKey" "Test" Nothing Nothing
  x1 <- I.putKey ccManager (I.hostWithScheme _client) ccPort put1 Nothing
  assertEqual "testDeleteKey: Write failed" True x1
  I.deleteKey ccManager (I.hostWithScheme _client) ccPort "/testDeleteKey" False Nothing
  x2 <- I.getKey ccManager (I.hostWithScheme _client) ccPort "/testDeleteKey" Nothing Nothing Nothing
  assertEqual "testDeleteKey: Key was not deleted" Nothing x2

{- Agent -}
testRegisterService :: TestTree
testRegisterService = testCase "testRegisterService" $ do
  _client@ConsulClient{..} <- client
  let req = RegisterService Nothing "testService" ["test"] Nothing (Just $ Ttl "10s")
  val <- I.registerService ccManager (I.hostWithScheme _client) ccPort req Nothing
  assertEqual "testRegisterService: Service was not created" val True

testGetSelf :: TestTree
testGetSelf = testCase "testGetSelf" $ do
  _client@ConsulClient{..} <- client
  x <- I.getSelf ccManager (I.hostWithScheme _client) ccPort
  assertEqual "testGetSelf: Self not returned" True (isJust x)

{-
testRegisterHealthCheck :: TestTree
testRegisterHealthCheck = testCase "testRegisterHealthCheck" $ do
  _client@ConsulClient{..} <- client
  let check = RegisterHealthCheck "testHealthCheck" "testHealthCheck" "" Nothing Nothing (Just "15s")
  x1 <- I.registerHealthCheck ccManager (I.hostWithScheme _client) ccPort check
  undefined -}

{- Health Checks -}
testGetServiceHealth :: TestTree
testGetServiceHealth = testCase "testGetServiceHealth" $ do
  _client@ConsulClient{..} <- client
  let req = RegisterService (Just "testGetServiceHealth") "testGetServiceHealth" [] Nothing Nothing
  r1 <- I.registerService ccManager (I.hostWithScheme _client) ccPort req Nothing
  case r1 of
    True -> do
      liftIO $ threadDelay 1000000
      r2 <- I.getServiceHealth ccManager (I.hostWithScheme _client) ccPort "testGetServiceHealth"
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
  _client@ConsulClient{..} <- client
  let req = SessionRequest Nothing (Just "testCreateSession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme _client) ccPort req Nothing
  case result of
    Just _ -> return ()
    Nothing -> assertFailure "testCreateSession: No session was created"

testGetSessionInfo :: TestTree
testGetSessionInfo = testCase "testGetSessionInfo" $ do
  _client@ConsulClient{..} <- client
  let req = SessionRequest Nothing (Just "testGetSessionInfo") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme _client) ccPort req Nothing
  case result of
    Just x -> do
      x1 <- I.getSessionInfo ccManager (I.hostWithScheme _client) ccPort (sId x) Nothing
      case x1 of
        Just _ -> return ()
        Nothing -> assertFailure "testGetSessionInfo: Session Info was not returned"
    Nothing -> assertFailure "testGetSessionInfo: No session was created"

testRenewSession :: TestTree
testRenewSession = testCase "testRenewSession" $ do
  _client@ConsulClient{..} <- client
  let req = SessionRequest Nothing (Just "testRenewSession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme _client) ccPort req Nothing
  case result of
    Just x -> do
      x1 <- I.renewSession ccManager (I.hostWithScheme _client) ccPort x Nothing
      case x1 of
        True -> return ()
        False -> assertFailure "testRenewSession: Session was not renewed"
    Nothing -> assertFailure "testRenewSession: No session was created"

testDestroySession :: TestTree
testDestroySession = testCase "testDestroySession" $ do
  _client@ConsulClient{..} <- client
  let req = SessionRequest Nothing (Just "testDestroySession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession ccManager (I.hostWithScheme _client) ccPort req Nothing
  case result of
    Just x -> do
      _ <- I.destroySession ccManager (I.hostWithScheme _client) ccPort x Nothing
      x1 <- I.getSessionInfo ccManager (I.hostWithScheme _client) ccPort (sId x) Nothing
      assertBool "testDestroySession: Session info was returned after destruction" $ (x1 == Nothing) || (x1 == Just [])
    Nothing -> assertFailure "testDestroySession: No session was created"

testInternalSession :: TestTree
testInternalSession = testGroup "Internal Session Tests" [testCreateSession, testGetSessionInfo, testRenewSession, testDestroySession]

{- Managed Session -}
testCreateManagedSession :: TestTree
testCreateManagedSession = testCase "testCreateManagedSession" $ do
  client <- initializeConsulClient "localhost" 8500 Nothing
  x <- createManagedSession client (Just "testCreateManagedSession") "60s"
  assertEqual "testCreateManagedSession: Session not created" True (isJust x)

testSessionMaintained :: TestTree
testSessionMaintained = testCase "testSessionMaintained" $ do
  client <- initializeConsulClient "localhost" 8500 Nothing
  x <- createManagedSession client (Just "testCreateManagedSession") "10s"
  assertEqual "testSessionMaintained: Session not created" True (isJust x)
  let (Just foo) = x
  threadDelay (12 * 1000000)
  y <- getSessionInfo client (sId $ msSession foo) Nothing
  assertEqual "testSessionMaintained: Session not found" True (isJust y)

testWithSessionCancel :: TestTree
testWithSessionCancel = testCase "testWithSessionCancel" $ do
  client@ConsulClient{..} <- initializeConsulClient "localhost" 8500 Nothing
  let req = SessionRequest Nothing (Just "testWithSessionCancel") Nothing ["serfHealth"] (Just Release) (Just "10s")
  result <- I.createSession ccManager (I.hostWithScheme client) ccPort req Nothing
  case result of
    Just x -> do
      x1 <- withSession client x (\ y -> action y client ) cancelAction
      assertEqual "testWithSessionCancel: Incorrect value" "Canceled" x1
    Nothing -> assertFailure "testWithSessionCancel: No session was created"
  where
    action x client@ConsulClient{..} = do
      I.destroySession ccManager (I.hostWithScheme client) ccPort x Nothing
      threadDelay (30 * 1000000)
      return ("NotCanceled" :: Text)
    cancelAction = return ("Canceled" :: Text)


managedSessionTests :: TestTree
managedSessionTests = testGroup "Managed Session Tests" [ testCreateManagedSession, testSessionMaintained, testWithSessionCancel]

agentTests :: TestTree
agentTests = testGroup "Agent Tests" [testGetSelf,testRegisterService]

allTests :: TestTree
allTests = testGroup "All Tests" [testInternalSession, internalKVTests, managedSessionTests, agentTests,testHealth]

main :: IO ()
main = defaultMain allTests
