{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Sequencer
  ( getSequencerForLock
  , isValidSequencer
  , withSequencer
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
import Network.Consul.Client.KVStore

-- | TODO: Document
getSequencerForLock :: MonadIO m => ConsulClient -> Text -> Session -> m (Maybe Sequencer)
getSequencerForLock client key session = do
  let dc = ccDatacenter client
  kv <- getKey client key Nothing (Just Consistent)
  case kv of
    Just k -> do
      let isValid = maybe False ((sId session) ==) $ kvSession k
      if isValid then return $ Just $ Sequencer key (kvLockIndex k) session else return Nothing
    Nothing -> return Nothing


-- | TODO: Document
isValidSequencer :: MonadIO m => ConsulClient -> Sequencer -> m Bool
isValidSequencer client sequencer = do
  mkv <- getKey client (sKey sequencer) Nothing (Just Consistent)
  case mkv of
    Just kv -> return $ (maybe False ((sId $ sSession sequencer) ==) $ kvSession kv) && (kvLockIndex kv) == (sLockIndex sequencer)
    Nothing -> return False


-- | TODO: Document
withSequencer :: (MonadMask m, MonadUnliftIO m) => ConsulClient -> Sequencer -> m a -> m a -> Int -> m a
withSequencer client sequencer action lostAction delay =
  withAsync action $ \ mainAsync -> withAsync pulseLock $ \ pulseAsync -> do
    waitAnyCancel [mainAsync, pulseAsync] >>= return . snd
  where
    pulseLock = recoverAll (exponentialBackoff 50000 <>  limitRetries 5) $ \ _ -> do
      liftIO $ threadDelay delay
      valid <- isValidSequencer client sequencer
      case valid of
        True -> pulseLock
        False -> lostAction
