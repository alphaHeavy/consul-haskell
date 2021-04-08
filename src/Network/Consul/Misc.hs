{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Misc
  ( runService
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
import Network.Consul.Client.Health
import Network.Consul.Client.Catalog


-- | TODO: Document
runService :: MonadUnliftIO m => ConsulClient -> RegisterService -> m () -> m ()
runService client request action = do
  r <- registerService client request
  case r of
    True -> do
      mainFunc <- async action

      --this is here instead of the where to prevent typechecking nastiness
      checkAction <- case rsCheck request of
                      Just(x@(Ttl _)) -> do
                        a <- async $ forever $ ttlFunc x
                        return $ Just a
                      _ -> return Nothing

      _foo :: () <- wait mainFunc --prevent: 'StM’ is a type function, and may not be injective
      case checkAction of
        Just a -> cancel a
        Nothing -> return ()
    False -> return ()
  where
    ttlFunc y@(Ttl x) = do
      let ttl = parseTtl x
      liftIO $ threadDelay $ (ttl - (fromIntegral $ floor (fromIntegral ttl / fromIntegral 2))) * 1000000
      let checkId = T.concat["service:",maybe (rsName request) id (rsId request)]
      passHealthCheck client checkId
