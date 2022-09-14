{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
  ( checkIds
  , consulPort
  , dc1
  , fiveSecondMicros
  , newClient
  , withProcessTerm
  , waitForConsulOrFail 
  , sleep
  , localhost
  , localNode
  , lockDelay
  , withSystemTempFile
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified System.Process.Typed as PT
import Control.Concurrent
import Control.Monad (when)
import Control.Retry
import Data.Text (unpack, Text)
import Network.Socket (PortNumber)
import SocketUtils (isPortOpen, simpleSockAddr)
import System.IO (hFlush)
import System.Process.Typed (proc)
import UnliftIO.Temporary (withSystemTempFile)

import Network.Consul.Internal
import Network.Consul.Types
import Network.Consul (initializeConsulClient)

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
newClient :: IO ConsulClient
newClient = initializeConsulClient localhost consulPort emptyHttpManager Nothing



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
