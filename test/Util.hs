{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util
  ( ConsulServerHandle (..)
  , TestsuiteSettings (..)
  , checkIds
  , consulPort
  , consulServerSetupFunc
  , consulServerSetupFuncWith
  , dc1
  , fiveSecondMicros
  , newClient
  , withProcessTerm
  , waitForConsulOrFail
  , sleep
  , localhost
  , localNode
  , lockDelay
  , UnliftIO.Temporary.withSystemTempFile
  , withConsulServer
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified System.Process.Typed as PT

import Prelude
  ( Bool(..)
  , Double
  , Either(..)
  , Int
  , IO
  , Maybe(..)
  , Show
  , Eq
  , ceiling
  , error
  , fromIntegral
  , not
  , print
  , pure
  , return
  , show
  , (<*>)
  , (<$>)
  , ($)
  , (*)
  , (++)
  , (<>)
  , (>>)
  , (.)
  , (==)
  , const
  , filter
  , undefined
  )
import Control.Concurrent
import Control.Monad (mzero, when)
import Control.Retry
import Data.ByteString (concat)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Base64.Lazy (decode)
import Data.Text (pack, unpack, Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (brConsume, parseUrlThrow, responseBody, withResponse)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Socket (PortNumber)
import SocketUtils (isPortOpen, simpleSockAddr)
import System.IO (hFlush)
import System.Process.Typed (proc, setWorkingDir, nullStream, setStderr, setStdout)
import UnliftIO.Temporary (withSystemTempFile)

-- for consul leadership query
import Data.Aeson as JSON
import Network.HTTP.Client (httpLbs, responseStatus)
import Network.HTTP.Types (ok200)

import Network.Consul.Internal
import Network.Consul.Types
import Network.Consul (initializeConsulClient)

-- new imports for new consul setup functions
import Test.Syd -- (SetupFunc)
import Test.Syd.Path -- (tempBinaryFileWithContentsSetupFunc)
import Test.Syd.Process.Typed -- (typedProcessSetupFunc)
import Network.Socket.Free
import Network.Socket.Wait
--import System.Process.Typed -- (TODO)
import Path
import Path.IO

data TestsuiteSettings =
  TestsuiteSettings
    { displayConsulServerLogs :: Bool
    , enableAcls :: Bool
    , verboseLogs :: Bool
    }

data ConsulCatalogNode =
  ConsulCatalogNode
    { consulCatalogNodeName :: Text
    , consulCatalogNodeAddress :: Text
    , consulCatalogNodeDatacenter :: Datacenter
    } deriving (Generic, Show, Eq)

instance FromJSON ConsulCatalogNode where
  parseJSON (Object o) =
    ConsulCatalogNode
      <$> o .: "Node"
      <*> o .: "Address"
      <*> o .: "Datacenter"
  parseJSON _ = mzero

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

dc1 :: Maybe Datacenter
dc1 = Just $ Datacenter "dc1"

-- Initialize a new `ConsulClient`.
newClient :: PortNumber -> IO ConsulClient
newClient consulPort = initializeConsulClient localhost consulPort emptyHttpManager Nothing
-- (pack ((unpack localhost) <> "-" <> (show consulPort)))



-- Backwards compatible `withProcessTerm`.
withProcessTerm
  :: PT.ProcessConfig stdin stdout stderr
  -> (PT.Process stdin stdout stderr -> IO a)
  -> IO a
-- #if MIN_VERSION_typed_process(0,2,5)
withProcessTerm = PT.withProcessTerm
-- #else
-- withProcessTerm = PT.withProcess
-- #endif

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


--withConsulServer :: ( -> IO ()) -> IO ()
withConsulServer app = do
  -- We use a non-standard port in the test suite and spawn consul there,
  -- to ensure that the test suite doesn't mess with real consul deployments.
  UnliftIO.Temporary.withSystemTempFile "haskell-consul-test-config.json" $ \configFilePath h -> do
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
    Util.withProcessTerm consulProc $ \_p -> do
      waitForConsulOrFail
      -- to let the consul agent register itself (the node the agent is running on)
      -- TODO: should we instead query consul to lookup the node registration?
      sleep 3

--
-- Regarding why we set an explicit node name (via `-node`) when running consul:
--
-- When we create a session, we need to reference a Node that has been
-- registered in Consul's node catalog. By telling the agent to use localhost,
-- after the agent boots, we can expect that the agent has registered a node for
-- itself and that the node's name is localhost, so that when we create a session,
-- we can simply reference that existing/registered node from the agent instead
-- of having to make up and register a Node for the test.

data ConsulServerHandle = ConsulServerHandle
  { consulServerHandleDnsPort :: !PortNumber
  , consulServerHandleGrpcPort :: !PortNumber
  , consulServerHandleHttpPort :: !PortNumber
  , consulServerHandleRpcPort :: !PortNumber
  , consulServerHandleSerfLanPort :: !PortNumber
  , consulServerHandleSerfWanPort :: !PortNumber
  , consulServerHandleNodeName :: !Node
  }


defaultTestsuiteSettings =
  TestsuiteSettings
    { displayConsulServerLogs = False
    , enableAcls = False
    , verboseLogs = False
    }

-- TODO: docs explain testsuite settings
consulServerSetupFunc :: SetupFunc ConsulServerHandle
consulServerSetupFunc = consulServerSetupFuncWith defaultTestsuiteSettings

-- TODO: how do we test this to make sure it is not faulty?
consulServerSetupFuncWith :: TestsuiteSettings -> SetupFunc ConsulServerHandle
consulServerSetupFuncWith settings = do
  tempDir <- tempDirSetupFunc "consul-server"
  dnsPortInt <- liftIO getFreePort
  grpcPortInt <- liftIO getFreePort
  httpPortInt <- liftIO getFreePort
  rpcPortInt <- liftIO getFreePort
  serfLanPortInt <- liftIO getFreePort
  serfWanPortInt <- liftIO getFreePort

  -- consul agent config (in hcl), default with no acls enabled
  let defaultConsulConfig = "disable_update_check = true\n"

  -- consul agent config (in hcl), with acls enabled
  -- TODO: render as HCL or JSON and not some string with \n in it?
  -- TODO: default acl policy should be deny, or configurable?
  -- When acl policy is deny, we're unable to boot b/c node registration fails due to deny policy
  let consulConfigAclsEnabled = "disable_update_check = true\nacl = {\nenabled = true\ndefault_policy = \"allow\"\nenable_token_persistence = true\n}"

  -- use default consul config file if acls not enabled, else if enabled, use the special config
  let consulConfigFileContents =
        case (enableAcls settings) of
          True -> consulConfigAclsEnabled
          False -> defaultConsulConfig

  configFilePath <- tempBinaryFileWithContentsSetupFunc
    "consul-test-config"
    consulConfigFileContents
  let nodeName = ((unpack localhost) <> "-" <> (show httpPortInt)) -- node name is localhost-${PORT}

  -- TODO: move these definitions outside or away from this block
  -- define options for how we run consul
  let defaultConsulServerCliArgs = 
        [ "agent"
        , "-node", nodeName
        , "-dns-port", show dnsPortInt
        , "-http-port", show httpPortInt -- (fromIntegral consulPort :: Int)
        , "-grpc-port", show grpcPortInt -- (fromIntegral consulPort :: Int)
        , "-server-port", show rpcPortInt
        , "-serf-lan-port", show serfLanPortInt
        , "-serf-wan-port", show serfWanPortInt
        , "-config-format", "hcl"
        , "-config-file", (fromAbsFile configFilePath)
        ]

  -- atm we run _either_ dev mode _or_ with acls enabled
  let devModeCliArgs =
        [ "-dev"
        ]

  let aclsEnabledCliArgs =
        [ "-bootstrap-expect=1"
        , "-bind=127.0.0.1"
        , ("-data-dir=" ++ (filter (not . (== '"')) (show tempDir)))
        , "-server=true" -- ^ drop the extra " from `show Path`
        ]

  -- TODO, set this more directly
  let logLevelErrorCliArgs =
        [ "-log-level", "err"
        ]

  let logLevelDebugCliArgs =
        [ "-log-level", "debug"        -- for debugging
        ]

  -- combine default cli args with dev mode or acls enabled, plus log-level settings
  let consulCliArgs =
        (case (enableAcls settings) of
          True -> defaultConsulServerCliArgs ++ aclsEnabledCliArgs
          False -> defaultConsulServerCliArgs ++ devModeCliArgs
        ) ++
        (case (verboseLogs settings) of
           True -> logLevelDebugCliArgs
           False -> logLevelErrorCliArgs
        )
  
  --liftIO $ print $ pack $ filter (not . (== '"')) (show tempDir)
  -- print cli args we're going to use to launch consul with
  case (verboseLogs settings) of
    True -> liftIO $ print consulCliArgs
    False -> pure()

  -- setup process config ("how to run consul")
  let processConfig =
        -- do we show the consul logs to testsuite stdout or hide it?
        case (displayConsulServerLogs settings) of
          True -> 
            setWorkingDir (fromAbsDir tempDir) $
              proc
                "consul"
                consulCliArgs
          False ->
            setStdout nullStream $
              setStderr nullStream $
                setWorkingDir (fromAbsDir tempDir) $
                  proc
                    "consul"
                    consulCliArgs
  -- run consul!
  _ <- typedProcessSetupFunc processConfig

  -- wait until consul is listening on all ports we've provided it
  liftIO $ wait "127.0.0.1" dnsPortInt
  liftIO $ wait "127.0.0.1" httpPortInt
  liftIO $ wait "127.0.0.1" grpcPortInt
  liftIO $ wait "127.0.0.1" rpcPortInt
  liftIO $ wait "127.0.0.1" serfLanPortInt
  liftIO $ wait "127.0.0.1" serfWanPortInt

  -- ping consul to make sure leadership has settled and agent is ready for work
  liftIO $ waitForLeadership httpPortInt

  -- ping consul to make sure the agent has registered itself as a node
  liftIO $ waitForNodeRegistration httpPortInt

  -- create our handle data structure, which is passed to tests this setupFunc wraps
  let consulServerHandleDnsPort = fromIntegral dnsPortInt
      consulServerHandleGrpcPort = fromIntegral rpcPortInt
      consulServerHandleHttpPort = fromIntegral httpPortInt
      consulServerHandleRpcPort = fromIntegral rpcPortInt
      consulServerHandleSerfLanPort = fromIntegral serfLanPortInt
      consulServerHandleSerfWanPort = fromIntegral serfWanPortInt
      consulServerHandleNodeName = Node (pack nodeName) localNodeAddr
  pure ConsulServerHandle {..}


--
-- Block while we wait for the Consul agent to have itself registered as a node
-- in the consul cluster.
--
-- This is to address test failures due to the following situation:
-- ... [ERROR] agent.http: Request error: method=PUT
--                                        url=/v1/session/create
--                                        from=127.0.0.1:54248
--                                        error="Missing node registration"
--
-- After the Consul agent acquires leadership in the cluster, there is then
-- an unknown amount of time where no nodes are registered in the cluster,
-- and we need to delay our tests from running until there is a registered
-- node.  This function blocks while we wait for that process to complete.
waitForNodeRegistration :: Int -> IO ()
waitForNodeRegistration consulPort = do
  ready <- queryNodeRegistrationWithRetries consulPort
  if ready then pure () else expectationFailure "Exceeded retry limits waiting for node registration"

-- Repeatedly query the Consul agent's node catalog API until we see a Node
-- registered in the catalog. Retry 100 times with a 50ms pause between retries.
queryNodeRegistrationWithRetries :: Int -> IO Bool
queryNodeRegistrationWithRetries consulPort =
  retrying
      (constantDelay 50000 <> limitRetries 1000) -- 1000 times, 50 ms each, for 50s max total
      (const $ pure . not)
      (const $ queryNodeRegistrationOnce consulPort)

-- Query the Consul Node Catalog API once, parsing the response and
-- failing with expectationFailure if we cannot decode the JSON response.
-- Return False if the list of nodes is empty. Return True if there are
-- entries and the Http response code was 200.
queryNodeRegistrationOnce :: Int -> IO Bool
queryNodeRegistrationOnce consulPort = do
  let consulNodeCatalogUrl = "http://localhost:" <> (pack $ show consulPort) <> "/v1/catalog/nodes"
  request <- parseUrlThrow $ unpack $ consulNodeCatalogUrl
  manager <- newTlsManager
  response <- httpLbs request manager
  let body = responseBody response
  -- TODO: evaluate settings and do or skip this
  --liftIO $ print $ responseBody response
  case JSON.eitherDecode body of
    Left e -> expectationFailure $ "node registration check: could not decode the json response: " ++ e ++ (show body)
    Right nodeList -> do
      case (nodeList :: [ConsulCatalogNode]) of
        [] -> pure False
        nodes -> do
          pure $ responseStatus response == ok200


-- Block while we wait for the Consul agent to acquire leader status.
--
-- After the Consul agent starts and has opened the TCP ports it will listen on,
-- the agent needs an unknown amount of time to sort out its internal raft
-- consensus and acquire leader status. If we run fast and try to query the
-- agent before it has an active leader, our queries will fail with HTTP 500, No
-- Cluster Leader. This function blocks while we wait for that process to
-- complete.
waitForLeadership :: Int -> IO ()
waitForLeadership consulPort = do
  ready <- queryLeadershipWithRetries consulPort
  if ready then pure () else expectationFailure "Exceeded retry limits waiting for consul leader"

-- Repeatedly query the Consul agent's leader status API until that query is
-- successful. Retry 100 times with a 50ms pause between retries.
queryLeadershipWithRetries :: Int -> IO Bool
queryLeadershipWithRetries consulPort =
  retrying
      (constantDelay 50000 <> limitRetries 1000) -- 1000 times, 50 ms each, for 50s max
      (const $ pure . not)
      (const $ queryLeadershipOnce consulPort)


-- TODO: how do we test this to confirm it's not flawed?
--
-- TODO: should this also ensure the node registration is complete? Or is that
--       another function entirely?
--
-- Query the Consul agent's leader status API once, parsing the response and
-- failing if we cannot decode the JSON response and if we don't see ah HTTP
-- 200 response code. Return expectationFailure on fail or True on success.
queryLeadershipOnce :: Int -> IO Bool
queryLeadershipOnce consulPort = do
  let consulStatusUrl = "http://localhost:" <> (pack $ show consulPort) <> "/v1/status/leader"
  request <- parseUrlThrow $ unpack $ consulStatusUrl
  manager <- newTlsManager -- emptyHttpManager
  response <- httpLbs request manager
  case JSON.decode $ responseBody response :: Maybe Text of
    Nothing -> expectationFailure "leadership check: could not decode the json"
    Just msg -> do
      --liftIO $ print ("JSON response from consul leader: " <> msg)
      pure $ responseStatus response == ok200
