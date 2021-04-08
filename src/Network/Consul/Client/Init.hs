{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Client.Init
  ( initializeConsulClient
  , initializeTlsConsulClient
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
initializeConsulClient :: MonadIO m => Text -> PortNumber -> Maybe Manager -> m ConsulClient
initializeConsulClient hostname port man = do
  manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newTlsManager
  return $ ConsulClient manager hostname port False Nothing
                                                 -- we omit a Datacenter here for brevity
                                                 -- it's still allowed via record updates

-- | TODO: Document
initializeTlsConsulClient :: MonadIO m => Text -> PortNumber -> Maybe Manager -> m ConsulClient
initializeTlsConsulClient hostname port man = do
    manager <- liftIO $ case man of
                        Just x -> return x
                        Nothing -> newTlsManagerWith tlsManagerSettings
    return $ ConsulClient manager hostname port True Nothing

