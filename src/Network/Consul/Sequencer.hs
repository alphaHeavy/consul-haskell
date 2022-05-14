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

import Import

import Network.Consul.Client.KVStore

-- | TODO: Document
getSequencerForLock :: MonadIO m => ConsulClient -> KeyPath -> Session -> m (Maybe Sequencer)
getSequencerForLock client key session = do
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
