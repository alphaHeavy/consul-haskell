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

manager :: IO Manager
manager = newManager defaultManagerSettings

{- Internal Tests -}
internalKVTests :: TestTree
internalKVTests = testGroup "Internal Key Value" [testGetInvalidKey, testPutKey, 
  testGetKey,testGetKeys,testListKeys,testDeleteKey]

testGetInvalidKey :: TestTree
testGetInvalidKey = testCase "testGetInvalidKey" $ do
  man <- manager
  x <- I.getKey man "localhost" (PortNum 8500) "nokey" Nothing Nothing Nothing
  assertEqual "testGetInvalidKey: Found a key that doesn't exist" x Nothing

testPutKey :: TestTree
testPutKey = testCase "testPutKey" $ do
  man <- manager
  let put = KeyValuePut "/testPutKey" "Test" Nothing Nothing
  x <- I.putKey man "localhost" (PortNum 8500) put Nothing
  assertEqual "testPutKey: Write failed" True x

testGetKey :: TestTree
testGetKey = testCase "testGetKey" $ do
  man <- manager
  let put = KeyValuePut "/testGetKey" "Test" Nothing Nothing
  x1 <- I.putKey man "localhost" (PortNum 8500) put Nothing
  assertEqual "testGetKey: Write failed" True x1
  x2 <- I.getKey man "localhost" (PortNum 8500) "/testGetKey" Nothing Nothing Nothing
  case x2 of
    Just x -> assertEqual "testGetKey: Incorrect Value" (kvValue x) "Test"
    Nothing -> assertFailure "testGetKey: No value returned"

testGetKeys :: TestTree
testGetKeys = testCase "testGetKeys" $ do
  man <- manager
  let put1 = KeyValuePut "/testGetKeys/key1" "Test" Nothing Nothing
  x1 <- I.putKey man "localhost" (PortNum 8500) put1 Nothing
  assertEqual "testGetKeys: Write failed" True x1
  let put2 = KeyValuePut "/testGetKeys/key2" "Test" Nothing Nothing
  x2 <- I.putKey man "localhost" (PortNum 8500) put2 Nothing
  assertEqual "testGetKeys: Write failed" True x2
  x3 <- I.getKeys man "localhost" (PortNum 8500) "/testGetKeys" Nothing Nothing Nothing
  assertEqual "testGetKeys: Incorrect number of results" 2 (length x3)

testListKeys :: TestTree
testListKeys = testCase "testListKeys" $ do
  man  <- manager
  let put1 = KeyValuePut "/testListKeys/key1" "Test" Nothing Nothing
  x1 <- I.putKey man "localhost" (PortNum 8500) put1 Nothing
  assertEqual "testListKeys: Write failed" True x1
  let put2 = KeyValuePut "/testListKeys/key2" "Test" Nothing Nothing
  x2 <- I.putKey man "localhost" (PortNum 8500) put2 Nothing
  assertEqual "testListKeys: Write failed" True x2
  x3 <- I.listKeys man "localhost" (PortNum 8500) "/testListKeys/" Nothing Nothing Nothing
  assertEqual "testListKeys: Incorrect number of results" 2 (length x3)

testDeleteKey :: TestTree
testDeleteKey = testCase "testDeleteKey" $ do
  man  <- manager
  let put1 = KeyValuePut "/testDeleteKey" "Test" Nothing Nothing
  x1 <- I.putKey man "localhost" (PortNum 8500) put1 Nothing
  assertEqual "testDeleteKey: Write failed" True x1
  I.deleteKey man "localhost" (PortNum 8500) "/testDeleteKey" False Nothing
  x2 <- I.getKey man "localhost" (PortNum 8500) "/testDeleteKey" Nothing Nothing Nothing
  assertEqual "testDeleteKey: Key was not deleted" Nothing x2

{- Agent -}
testRegisterService :: TestTree
testRegisterService = testCase "testRegisterService" $ do
  man <- manager
  let req = RegisterService Nothing "testService" ["test"] Nothing (Right "10s")
  I.registerService man "localhost" (PortNum 8500) req
{-
testRegisterHealthCheck :: TestTree
testRegisterHealthCheck = testCase "testRegisterHealthCheck" $ do
  man <- manager
  let check = RegisterHealthCheck "testHealthCheck" "testHealthCheck" "" Nothing Nothing (Just "15s")
  x1 <- I.registerHealthCheck man "localhost" (PortNum 8500) check
  undefined -}

{- Health Checks -}
--testServiceChecks :: 

{- Session -}
testCreateSession :: TestTree
testCreateSession = testCase "testCreateSession" $ do
  man <- manager
  let req = SessionRequest Nothing (Just "testCreateSession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession man "localhost" (PortNum 8500) req Nothing
  case result of
    Just _ -> return ()
    Nothing -> assertFailure "testCreateSession: No session was created"

testGetSessionInfo :: TestTree
testGetSessionInfo = testCase "testGetSessionInfo" $ do
  man <- manager
  let req = SessionRequest Nothing (Just "testGetSessionInfo") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession man "localhost" (PortNum 8500) req Nothing
  case result of
    Just x -> do
      x1 <- I.getSessionInfo man "localhost" (PortNum 8500) (sId x) Nothing
      case x1 of
        Just _ -> return ()
        Nothing -> assertFailure "testGetSessionInfo: Session Info was not returned"
    Nothing -> assertFailure "testGetSessionInfo: No session was created"

testRenewSession :: TestTree
testRenewSession = testCase "testRenewSession" $ do
  man <- manager
  let req = SessionRequest Nothing (Just "testRenewSession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession man "localhost" (PortNum 8500) req Nothing
  case result of
    Just x -> do
      x1 <- I.renewSession man "localhost" (PortNum 8500) x Nothing
      case x1 of
        True -> return ()
        False -> assertFailure "testRenewSession: Session was not renewed"
    Nothing -> assertFailure "testRenewSession: No session was created"

testDestroySession :: TestTree
testDestroySession = testCase "testDestroySession" $ do
  man <- manager
  let req = SessionRequest Nothing (Just "testDestroySession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  result <- I.createSession man "localhost" (PortNum 8500) req Nothing
  case result of
    Just x -> do
      _ <- I.destroySession man "localhost" (PortNum 8500) x Nothing
      x1 <- I.getSessionInfo man "localhost" (PortNum 8500) (sId x) Nothing
      assertEqual "testDestroySession: Session info was returned after destruction" Nothing x1
    Nothing -> assertFailure "testDestroySession: No session was created"

testInternalSession :: TestTree
testInternalSession = testGroup "Internal Session Tests" [testCreateSession, testGetSessionInfo, testRenewSession, testDestroySession]

{- Managed Session -}
testCreateManagedSession :: TestTree
testCreateManagedSession = testCase "testCreateManagedSession" $ do
  client <- initializeConsulClient "localhost" (PortNum 8500) Nothing
  x <- createManagedSession client (Just "testCreateManagedSession") "60s"
  assertEqual "testCreateManagedSession: Session not created" True (isJust x)

testSessionMaintained :: TestTree
testSessionMaintained = testCase "testSessionMaintained" $ do
  client <- initializeConsulClient "localhost" (PortNum 8500) Nothing
  x <- createManagedSession client (Just "testCreateManagedSession") "10s"
  assertEqual "testSessionMaintained: Session not created" True (isJust x)
  let (Just foo) = x
  threadDelay (12 * 1000000)
  y <- getSessionInfo client (sId $ msSession foo) Nothing
  assertEqual "testSessionMaintained: Session not found" True (isJust y)

testWithSessionCancel :: TestTree
testWithSessionCancel = testCase "testWithSessionCancel" $ do
  man <- manager
  client <- initializeConsulClient "localhost" (PortNum 8500) Nothing
  let req = SessionRequest Nothing (Just "testWithSessionCancel") Nothing ["serfHealth"] (Just Release) (Just "10s")
  result <- I.createSession man "localhost" (PortNum 8500) req Nothing
  case result of
    Just x -> do
      x1 <- withSession client x (action x man) cancelAction
      assertEqual "testWithSessionCancel: Incorrect value" "Canceled" x1
    Nothing -> assertFailure "testWithSessionCancel: No session was created"
  where
    action x man = do
      I.destroySession man "localhost" (PortNum 8500) x Nothing
      threadDelay (30 * 1000000)
      return ("NotCanceled" :: Text)
    cancelAction = return ("Canceled" :: Text)


managedSessionTests :: TestTree
managedSessionTests = testGroup "Managed Session Tests" [ testCreateManagedSession, testSessionMaintained, testWithSessionCancel]

allTests :: TestTree
allTests = testGroup "All Tests" [testInternalSession, internalKVTests, managedSessionTests]

main :: IO ()
main = defaultMain allTests
