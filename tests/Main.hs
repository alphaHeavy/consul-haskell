{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Retry
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe
#if MIN_VERSION_base(4,11,0)
-- (<>) is part of Prelude
#else
import Data.Monoid ((<>))
#endif
import Data.Text (unpack, Text)
import Data.UUID
import Network.Consul
  ( createSession
  , deleteKey
  , destroySession
  , getKey
  , getSequencerForLock
  , getSessionInfo
  , initializeConsulClient
  , isValidSequencer
  , putKey
  , putKeyAcquireLock
  , withSession
  , ConsulClient(..)
  , runService
  , getServiceHealth
  )
import Network.Consul.Types
import Network.Consul
import Network.Consul.Internal (hostWithScheme, emptyHttpManager)
import Network.HTTP.Client
import Network.Socket (PortNumber)
import System.IO (hFlush)
import System.Process.Typed (proc)
import qualified System.Process.Typed as PT
import System.Random
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.Temporary (withSystemTempFile)

import SocketUtils (isPortOpen, simpleSockAddr)

-- * Internal Helper Utilities

-- 5 seconds.
fiveSecondMicros :: Int
fiveSecondMicros = 5 * 1000 * 1000

-- Name of existing health check we can rely on for tests that use the Session API
serfHealth :: Text
serfHealth = "serfHealth"

-- list of names of service/node checks (which should exist in consul already)
checkIds :: [Text]
checkIds = [serfHealth]

-- for requests to session API
lockDelay :: Maybe a
lockDelay = Nothing

-- Sleep for N seconds with `threadDelay()`.
sleep :: Double -> IO ()
sleep seconds = threadDelay (ceiling (seconds * 1e6))

-- Define a `consulHost` for use in running tests against the Consul Agent
localhost :: ConsulHost
localhost = "localhost"

-- The IP Address of the local agent.
localNodeAddr :: Text
localNodeAddr = "127.0.0.1"

-- Instantiate a `ConsulHost` for these tests.
localNode :: Node
localNode = Node localhost localNodeAddr

-- The network port where the Consul Agent will listen for the HTTP API.
consulPort :: PortNumber
consulPort = 18500

dc1 :: Datacenter
dc1 = Datacenter "dc1"

-- Initialize a new `ConsulClient`.
newClient :: IO ConsulClient
newClient = initializeConsulClient localhost consulPort dc1 emptyHttpManager

{- Internal Tests -}
internalKVTests :: TestTree
internalKVTests =
  testGroup
    "Internal Key Value"
    [ testGetInvalidKey
    , testPutKey
    , testPutKeyAcquireLock
    , testPutKeyReleaseLock
    , testGetKey
    , testGetKeys
    , testListKeys
    , testDeleteKey
    , testGetNullValueKey
    , testDeleteRecursive
    ]

testGetInvalidKey :: TestTree
testGetInvalidKey = testCase "testGetInvalidKey" $ do
  client@ConsulClient{..} <- newClient
  x <- getKey client "nokey" Nothing Nothing
  assertEqual "testGetInvalidKey: Found a key that doesn't exist" x Nothing

testPutKey :: TestTree
testPutKey = testCase "testPutKey" $ do
  client@ConsulClient{..} <- newClient
  let put = KeyValuePut "/testPutKey" "Test" Nothing Nothing
  x <- putKey client put
  assertEqual "testPutKey: Write failed" True x

testPutKeyAcquireLock :: TestTree
testPutKeyAcquireLock = testCase "testPutKeyAcquireLock" $ do
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
      assertEqual "testPutKeyAcquireLock: Write failed" True x
      Just kv <- getKey client "/testPutKeyAcquireLock" Nothing Nothing
      let Just returnedSession = kvSession kv
      assertEqual "testPutKeyAcquireLock: Session was not found on key" returnedSession (sId session)

testPutKeyReleaseLock :: TestTree
testPutKeyReleaseLock = testCase "testPutKeyReleaseLock" $ do
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
      assertEqual "testPutKeyReleaseLock: Write failed" True x
      Just kv <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
      let Just returnedSession = kvSession kv
      assertEqual "testPutKeyReleaseLock: Session was not found on key" returnedSession (sId session)
      let put2 = KeyValuePut "/testPutKeyReleaseLock" "Test" Nothing Nothing
      x2 <- putKeyReleaseLock client put2 session
      assertEqual "testPutKeyReleaseLock: Release failed" True x2
      Just kv2 <- getKey client "/testPutKeyReleaseLock" Nothing Nothing
      assertEqual "testPutKeyAcquireLock: Session still held" Nothing (kvSession kv2)


testGetKey :: TestTree
testGetKey = testCase "testGetKey" $ do
  client@ConsulClient{..} <- newClient
  let put = KeyValuePut "/testGetKey" "Test" Nothing Nothing
  x1 <- putKey client put
  assertEqual "testGetKey: Write failed" True x1
  x2 <- getKey client "/testGetKey" Nothing Nothing
  case x2 of
    Just x -> assertEqual "testGetKey: Incorrect Value" (kvValue x) (Just "Test")
    Nothing -> assertFailure "testGetKey: No value returned"

testGetNullValueKey :: TestTree
testGetNullValueKey = testCase "testGetNullValueKey" $ do
  client@ConsulClient{..} <- newClient
  let put = KeyValuePut "/testGetNullValueKey" "" Nothing Nothing
  x1 <- putKey client put
  assertEqual "testGetNullValueKey: Write failed" True x1
  liftIO $ sleep 0.5
  x2 <- getKey client "/testGetNullValueKey" Nothing Nothing
  case x2 of
    Just x -> assertEqual "testGetNullValueKey: Incorrect Value" (kvValue x) Nothing
    Nothing -> assertFailure "testGetNullValueKey: No value returned"

testGetKeys :: TestTree
testGetKeys = testCase "testGetKeys" $ do
  client@ConsulClient{..} <- newClient
  let put1 = KeyValuePut "/testGetKeys/key1" "Test" Nothing Nothing
  x1 <- putKey client put1
  assertEqual "testGetKeys: Write failed" True x1
  let put2 = KeyValuePut "/testGetKeys/key2" "Test" Nothing Nothing
  x2 <- putKey client put2
  assertEqual "testGetKeys: Write failed" True x2
  x3 <- getKeys client "/testGetKeys" Nothing Nothing
  assertEqual "testGetKeys: Incorrect number of results" 2 (length x3)

testListKeys :: TestTree
testListKeys = testCase "testListKeys" $ do
  client@ConsulClient{..} <- newClient
  let put1 = KeyValuePut "/testListKeys/key1" "Test" Nothing Nothing
  x1 <- putKey client put1
  assertEqual "testListKeys: Write failed" True x1
  let put2 = KeyValuePut "/testListKeys/key2" "Test" Nothing Nothing
  x2 <- putKey client put2
  assertEqual "testListKeys: Write failed" True x2
  x3 <- listKeys client "/testListKeys/" Nothing Nothing
  assertEqual "testListKeys: Incorrect number of results" 2 (length x3)

testDeleteKey :: TestTree
testDeleteKey = testCase "testDeleteKey" $ do
  client@ConsulClient{..} <- newClient
  let put1 = KeyValuePut "/testDeleteKey" "Test" Nothing Nothing
  x1 <- putKey client put1
  assertEqual "testDeleteKey: Write failed" True x1
  x2 <- deleteKey client "/testDeleteKey" False
  assertEqual "testDeleteKey: Delete Failed" True x2
  x3 <- getKey client "/testDeleteKey" Nothing Nothing
  assertEqual "testDeleteKey: Key was not deleted" Nothing x3

testDeleteRecursive :: TestTree
testDeleteRecursive = testCase "testDeleteRecursive" $ do
  client@ConsulClient{..} <- newClient
  let put1 = KeyValuePut "/testDeleteRecursive/1" "Test" Nothing Nothing
      put2 = KeyValuePut "/testDeleteRecursive/2" "Test" Nothing Nothing
  x1 <- putKey client put1
  assertEqual "testDeleteKey: Write failed" True x1
  x2 <- putKey client put2
  assertEqual "testDeleteKey: Write failed" True x2
  deleteKey client "/testDeleteRecursive/" True
  x3 <- getKey client "/testDeleteRecursive/1" Nothing Nothing
  assertEqual "testDeleteKey: Key was not deleted" Nothing x3

{- Client KV -}
clientKVTests :: TestTree
clientKVTests = testGroup "Client KV Tests" [testDeleteRecursiveClient]

testDeleteRecursiveClient :: TestTree
testDeleteRecursiveClient = testCase "testDeleteRecursiveClient" $ do
  client <- newClient
  let put1 = KeyValuePut "/testDeleteRecursive/1" "Test" Nothing Nothing
      put2 = KeyValuePut "/testDeleteRecursive/2" "Test" Nothing Nothing
  x1 <- putKey client put1
  assertEqual "testDeleteKey: Write failed" True x1
  x2 <- putKey client put2
  assertEqual "testDeleteKey: Write failed" True x2
  deleteKey client "/testDeleteRecursive/" True
  x3 <- getKey client "/testDeleteRecursive/1" Nothing Nothing
  assertEqual "testDeleteKey: Key was not deleted" Nothing x3

{- Agent -}
testRegisterService :: TestTree
testRegisterService = testCase "testRegisterService" $ do
  client@ConsulClient{..} <- newClient
  let req = RegisterService Nothing "testService" ["test"] Nothing (Just $ Ttl "10s")
  val <- registerService client req
  assertEqual "testRegisterService: Service was not created" val True
  mService <- getService client "testService" Nothing
  let serviceWasNotFound = assertFailure "testRegisterService: Service was not found"
  case mService of
    Just [] -> serviceWasNotFound
    Nothing -> serviceWasNotFound
    Just _ -> return ()

testDeregisterService :: TestTree
testDeregisterService = testCase "testDeregisterService" $ do
  client@ConsulClient{..} <- newClient
  let req = RegisterService Nothing "testService" ["test"] Nothing Nothing
  val <- registerService client req Nothing
  assertEqual "testDeregisterService: Service was not created" val True
  deregisterService client (rsName req) Nothing
  mService <- getService client (rsName req) Nothing
  case mService of
    Just [] -> return ()
    Nothing -> return ()
    Just s -> assertFailure $ "testDeregisterService: Service was found... " <> show s

testGetSelf :: TestTree
testGetSelf = testCase "testGetSelf" $ do
  client@ConsulClient{..} <- newClient
  x <- getSelf client
  assertEqual "testGetSelf: Self not returned" True (isJust x)

{-
testRegisterHealthCheck :: TestTree
testRegisterHealthCheck = testCase "testRegisterHealthCheck" $ do
  client@ConsulClient{..} <- newClient
  let check = RegisterHealthCheck "testHealthCheck" "testHealthCheck" "" Nothing Nothing (Just "15s")
  x1 <- registerHealthCheck ccManager (hostWithScheme client) ccPort check
  undefined -}

{- Health Checks -}
testGetServiceHealth :: TestTree
testGetServiceHealth = testCase "testGetServiceHealth" $ do
  client@ConsulClient{..} <- newClient
  let req = RegisterService (Just "testGetServiceHealth") "testGetServiceHealth" [] Nothing Nothing
  r1 <- registerService client req
  case r1 of
    True -> do
      liftIO $ sleep 1
      r2 <- getServiceHealth client "testGetServiceHealth"
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
  let ttl = "30s"
      req =
        SessionRequest
          lockDelay
          (Just "testCreateSession")
          localNode
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
            sleep 0.05  -- pause for 50ms
            loopUntilSession
  result <- timeout fiveSecondMicros loopUntilSession
  case result of
    Just _ -> return ()
    Nothing -> assertFailure $ "testCreateSession: Session creation failed after retrying for 5 seconds"


testGetSessionInfo :: TestTree
testGetSessionInfo = testCase "testGetSessionInfo" $ do
  client@ConsulClient{..} <- newClient
  let ttl = "30s"
      req =
        SessionRequest
          lockDelay
          (Just "testGetSessionInfo")
          localNode
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
        Nothing -> assertFailure "testGetSessionInfo: Session Info was not returned"
    Nothing -> assertFailure "testGetSessionInfo: No session was created"

testRenewSession :: TestTree
testRenewSession = testCase "testRenewSession" $ do
  client@ConsulClient{..} <- newClient
  let ttl = "30s"
      req = SessionRequest Nothing (Just "testRenewSession") localNode checkIds (Just Release) (Just ttl)
  result <- createSession client req
  case result of
    Just x -> do
      x1 <- renewSession client x
      case x1 of
        True -> return ()
        False -> assertFailure "testRenewSession: Session was not renewed"
    Nothing -> assertFailure "testRenewSession: No session was created"

testRenewNonexistentSession :: TestTree
testRenewNonexistentSession = testCase "testRenewNonexistentSession" $ do
  client@ConsulClient{..} <- newClient
  sessId :: UUID <- randomIO
  let session = Session (toText sessId) Nothing
  x <- renewSession client session
  case x of
    True -> assertFailure "testRenewNonexistentSession: Non-existent session was renewed"
    False -> return ()

testDestroySession :: TestTree
testDestroySession = testCase "testDestroySession" $ do
  client@ConsulClient{..} <- newClient
  let ttl = "30s"
      req = SessionRequest Nothing (Just "testDestroySession") localNode checkIds (Just Release) (Just ttl)
  result <- createSession client req
  case result of
    Just x -> do
      _ <- destroySession client x
      x1 <- getSessionInfo client x
      assertBool "testDestroySession: Session info was returned after destruction" $ (x1 == Nothing) || (x1 == Just [])
    Nothing -> assertFailure "testDestroySession: No session was created"

testInternalSession :: TestTree
testInternalSession = testGroup "Internal Session Tests" [testCreateSession, testGetSessionInfo, testRenewSession, testRenewNonexistentSession, testDestroySession]

testSessionMaintained :: TestTree
testSessionMaintained = testCase "testSessionMaintained" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testSessionMaintained") localNode checkIds (Just Release) (Just "15s")
  result <- createSession client req
  case result of
    Just session -> do
      sleep 12
      y <- getSessionInfo client session
      assertEqual "testSessionMaintained: Session not found" True (isJust y)
    Nothing -> assertFailure "testSessionMaintained: No Session was created"


testWithSessionCancel :: TestTree
testWithSessionCancel = testCase "testWithSessionCancel" $ do
  client@ConsulClient{..} <- newClient
  let req = SessionRequest Nothing (Just "testWithSessionCancel") localNode checkIds (Just Release) (Just "10s")
  result <- createSession client req
  case result of
    Just session -> do
      x1 <- withSession client Nothing 5 session (\ y -> action y client ) cancelAction
      assertEqual "testWithSessionCancel: Incorrect value" "Canceled" x1
      z <- getSessionInfo client session
      assertBool "testWithSessionCancel: Session was found" $ (z == Nothing) || (z == Just [])
    Nothing -> assertFailure "testWithSessionCancel: No session was created"
  where
    action :: MonadIO m => Session -> ConsulClient -> m Text
    action x client@ConsulClient{..} = do
      destroySession client x
      liftIO $ sleep 30
      return ("NotCanceled" :: Text)

    cancelAction :: MonadIO m => m Text
    cancelAction = return ("Canceled" :: Text)


testRunServiceTtl :: TestTree
testRunServiceTtl = testCase "testRunServiceTtl" $ do
  client@ConsulClient{..} <- newClient
  let register = RegisterService Nothing "testRunServiceTtl" [] (Just 8000) $ Just $ Ttl "10s"
  runService client register (action client)
  where
    action client = do
      sleep 15
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

-- TODO: drop stringified values (localhost, dc1, etc)
testIsValidSequencer :: TestTree
testIsValidSequencer = testCase "testIsValidSequencer" $ do
  client@ConsulClient{..} <- initializeConsulClient localhost consulPort dc1 Nothing
  let req = SessionRequest Nothing (Just "testIsValidSequencer") localNode checkIds (Just Release) (Just "10s")
  result <- createSession client req
  case result of
    Nothing -> assertFailure "testIsValidSequencer: No session was created"
    Just session -> do
      let put = KeyValuePut "/testIsValidSequencer" "Test" Nothing Nothing
      x <- putKeyAcquireLock client put session
      assertEqual "testIsValidSequencer: Write failed" True x
      Just sequencer <- getSequencerForLock client "/testIsValidSequencer" session
      result1 <- isValidSequencer client sequencer
      assertEqual "testIsValidSequencer: Valid sequencer was invalid" True result1
      _ <- destroySession client session
      result2 <- isValidSequencer client sequencer
      assertEqual "testIsValidSequencer: Invalid session was valid" False result2


sessionWorkflowTests :: TestTree
sessionWorkflowTests =
  testGroup
    "Session Workflow Tests"
    [ testWithSessionCancel
    , testSessionMaintained
    ]

runServiceTests :: TestTree
runServiceTests =
  testGroup
    "Run Service Tests"
    [ testRunServiceTtl
    ]

agentTests :: TestTree
agentTests =
  testGroup
    "Agent Tests"
    [ testGetSelf
    , testRegisterService
    , testDeregisterService
    ]

sequencerTests :: TestTree
sequencerTests =
  testGroup
    "Sequencer Tests"
    [ testIsValidSequencer
    ]

allTests :: TestTree
allTests =
  testGroup
    "All Tests"
    [ testInternalSession
    , internalKVTests
    , sessionWorkflowTests
    , agentTests
    , testHealth
    , clientKVTests
    , runServiceTests
    , sequencerTests
    ]

-- Backwards compatible `withProcessTerm`.
withProcessTerm :: PT.ProcessConfig stdin stdout stderr -> (PT.Process stdin stdout stderr -> IO a) -> IO a
#if MIN_VERSION_typed_process(0,2,5)
withProcessTerm = PT.withProcessTerm
#else
withProcessTerm = PT.withProcess
#endif

waitForConsulOrFail :: IO ()
waitForConsulOrFail = do
  success <-
    retrying
      (constantDelay 50000 <> limitRetries 100) -- 100 times, 50 ms each
      (\_status isOpen -> return (not isOpen)) -- when to retry
      $ \_status -> do
        isPortOpen $ (simpleSockAddr (127,0,0,1) consulPort)
  when (not success) $ do
    error $ "Could not connect to Consul within reasonable time"

main :: IO ()
main = do
  -- We use a non-standard port in the test suite and spawn consul there,
  -- to ensure that the test suite doesn't mess with real consul deployments.
  withSystemTempFile "haskell-consul-test-config.json" $ \configFilePath h -> do
    BS8.hPutStrLn h "{ \"disable_update_check\": true }" >> hFlush h
    let consulProc =
          proc
            "consul"
            [ "agent", "-dev"
            , "-node", (unpack localhost) -- hardcode node name as "localhost" * see below
            , "-log-level", "err"
          --, "-log-level", "debug"        -- for debugging
            , "-http-port", show (fromIntegral consulPort :: Int)
            , "-config-file", configFilePath
            ]
    withProcessTerm consulProc $ \_p -> do
      waitForConsulOrFail
      -- to let the consul agent register itself (the node the agent is running on)
      -- TODO: should we instead query consul to lookup the node registration?
      sleep 3
      defaultMain allTests

--
-- Regarding why we set an explicit node name (via `-node`) when running consul:
--
-- When we create a session, we need to reference a Node that has been
-- registered in Consul's node catalog. By telling the agent to use localhost,
-- after the agent boots, we can expect that the agent has registered a node for
-- itself and that the node's name is localhost, so that when we create a session,
-- we can simply reference that existing/registered node from the agent instead
-- of having to make up and register a Node for the test.
