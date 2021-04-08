{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Client.Agent
  ( getSelf
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

{- Agent -}
getSelf :: MonadIO m => ConsulClient -> m (Maybe Self)
getSelf _client@ConsulClient{..} =  do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/agent/self"]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ decode $ BL.fromStrict body


