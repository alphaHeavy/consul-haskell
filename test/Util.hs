{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Util
  ( ConsulServerHandle (..)
  , checkIds
  , consulPort
  , consulServerSetupFunc 
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

import Control.Concurrent
import Control.Monad (when)
import Control.Retry
import Data.Text (pack, unpack, Text)
import Network.Socket (PortNumber)
import SocketUtils (isPortOpen, simpleSockAddr)
import System.IO (hFlush)
import System.Process.Typed (proc, setWorkingDir, nullStream, setStderr, setStdout)
import UnliftIO.Temporary (withSystemTempFile)

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
newClient consulPort = initializeConsulClient localhost consulPort emptyHttpManager
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
            "/home/user/bin/consul"
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

--withConsulServer :: (ClientEnv -> IO ()) -> IO ()
--withConsulServer = undefined

data ConsulServerHandle = ConsulServerHandle
  { consulServerHandleDnsPort :: !PortNumber
  , consulServerHandleGrpcPort :: !PortNumber
  , consulServerHandleHttpPort :: !PortNumber
  , consulServerHandleRpcPort :: !PortNumber
  , consulServerHandleSerfLanPort :: !PortNumber
  , consulServerHandleSerfWanPort :: !PortNumber
  , consulServerHandleNodeName :: !Node
  }

consulServerSetupFunc :: SetupFunc ConsulServerHandle 
consulServerSetupFunc = do
  tempDir <- tempDirSetupFunc "consul-server"
  dnsPortInt <- liftIO getFreePort
  grpcPortInt <- liftIO getFreePort
  httpPortInt <- liftIO getFreePort
  rpcPortInt <- liftIO getFreePort
  serfLanPortInt <- liftIO getFreePort
  serfWanPortInt <- liftIO getFreePort
  configFilePath <- tempBinaryFileWithContentsSetupFunc
    "consul-test-config"
    "{ \"disable_update_check\": true }"
  let nodeName = ((unpack localhost) <> "-" <> (show httpPortInt)) -- hardcode node name as "localhost" * see below
  -- setup process for "how to run consul"
  let processConfig =
        setStdout nullStream $
          setStderr nullStream $
            setWorkingDir (fromAbsDir tempDir) $
              proc
                "/home/user/bin/consul"
                [ "agent", "-dev"
                , "-node", nodeName
                , "-log-level", "err"
              --, "-log-level", "debug"        -- for debugging
                , "-dns-port", show dnsPortInt
                , "-http-port", show httpPortInt -- (fromIntegral consulPort :: Int)
                , "-grpc-port", show grpcPortInt -- (fromIntegral consulPort :: Int)
                , "-server-port", show rpcPortInt
                , "-serf-lan-port", show serfLanPortInt
                , "-serf-wan-port", show serfWanPortInt
                , "-config-file", (fromAbsFile configFilePath)
                ]
  -- run consul!
  _ <- typedProcessSetupFunc processConfig
  -- wait until consul is listening on all ports we've provided it
  liftIO $ wait "127.0.0.1" dnsPortInt
  liftIO $ wait "127.0.0.1" httpPortInt
  liftIO $ wait "127.0.0.1" grpcPortInt
  liftIO $ wait "127.0.0.1" rpcPortInt
  liftIO $ wait "127.0.0.1" serfLanPortInt
  liftIO $ wait "127.0.0.1" serfWanPortInt
  -- create our handle data structure, which is passed to tests this setupFunc wraps
  let consulServerHandleDnsPort = fromIntegral dnsPortInt
      consulServerHandleGrpcPort = fromIntegral rpcPortInt
      consulServerHandleHttpPort = fromIntegral httpPortInt
      consulServerHandleRpcPort = fromIntegral rpcPortInt
      consulServerHandleSerfLanPort = fromIntegral serfLanPortInt
      consulServerHandleSerfWanPort = fromIntegral serfWanPortInt
      consulServerHandleNodeName = Node (pack nodeName) localNodeAddr
  pure ConsulServerHandle {..}
