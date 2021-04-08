{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Client.Health
  ( deregisterHealthCheck
  , failHealthCheck
  , getServiceChecks 
  , getServiceHealth
  , passHealthCheck
  , registerHealthCheck
  , warnHealthCheck
  ) where

import Control.Concurrent hiding (killThread)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadMask)
import Control.Retry
import Data.Aeson (Value(..), decode,encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word
import qualified Data.Vector as V
import qualified Network.Consul.Internal as I
import Network.Consul.Types
import Network.HTTP.Client -- (method, Manager, responseBody)
import Network.HTTP.Client.TLS (newTlsManager, newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types
import Network.Socket (PortNumber)
import UnliftIO (MonadUnliftIO, async, cancel, finally, wait, waitAnyCancel, withAsync)

import Network.Consul.Internal

-- | TODO: Document
{-getHealthChecks :: MonadIO m => Manager -> Text -> PortNumber -> Maybe Datacenter -> m [Check]
getHealthChecks  manager hostname portNumber dc = do
  request <- createRequest hostname portNumber "/agent/checks" Nothing Nothing False dc
 -}

{- Health Checks -}
getServiceChecks :: MonadIO m => ConsulClient -> Text -> m [Check]
getServiceChecks _client@ConsulClient{..} name = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/health/checks", name])
                           Nothing
                           Nothing
                           False
                           Nothing
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ maybe [] id (decode $ BL.fromStrict body)


getServiceHealth :: MonadIO m => ConsulClient -> Text -> m (Maybe [Health])
getServiceHealth _client@ConsulClient{..} name = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/health/service/", name]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ decode $ BL.fromStrict body


-- | TODO: Document
registerHealthCheck :: MonadIO m => ConsulClient -> RegisterHealthCheck -> m ()
registerHealthCheck client@ConsulClient{..} request = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/agent/check/register"]
  let httpReq = initReq { method = "PUT", requestBody = RequestBodyBS $ BL.toStrict $ encode request}
  liftIO $ withResponse httpReq ccManager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

-- | TODO: Document
deregisterHealthCheck :: MonadIO m => ConsulClient -> Text -> m ()
deregisterHealthCheck client@ConsulClient{..} checkId = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/agent/check/deregister/", checkId])
                           Nothing
                           Nothing
                           False
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()


-- | TODO: Document
passHealthCheck :: MonadIO m => ConsulClient -> Text -> m ()
passHealthCheck client checkId = do
  -- Using `Just ""` as the `body` to ensure a PUT request is used.
  -- Consul < 1.0 accepted a GET here (which was a legacy mistake).
  -- In 1.0, they switched it to require a PUT.
  -- See also:
  --   * https://github.com/hashicorp/consul/issues/3659
  --   * https://github.com/cablehead/python-consul/pull/182
  --   * https://github.com/hashicorp/consul/blob/51ea240df8476e02215d53fbfad5838bf0d44d21/CHANGELOG.md
  --     Section "HTTP Verbs are Enforced in Many HTTP APIs":
  --     > Many endpoints in the HTTP API that previously took any HTTP verb
  --     > now check for specific HTTP verbs and enforce them.
  let portNumber = ccPort client
      manager = ccManager client
      hostname = hostWithScheme client
      dc = ccDatacenter client
      --hostname = checkId
  initReq <- createRequest hostname
                           portNumber
                           (T.concat ["/v1/agent/check/pass/", checkId])
                           Nothing
                           (Just "")
                           False
                           dc
  liftIO $ withResponse initReq manager $ \ _response -> do
    return ()



-- | TODO: Document
warnHealthCheck :: MonadIO m => ConsulClient -> Text -> m ()
warnHealthCheck client@ConsulClient{..} checkId = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/agent/check/warn/", checkId]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()


-- | TODO: Document
failHealthCheck :: MonadIO m => ConsulClient -> Text -> m ()
failHealthCheck client@ConsulClient{..} checkId = do
  let hostnameWithScheme = hostWithScheme client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/agent/check/fail/", checkId]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()

