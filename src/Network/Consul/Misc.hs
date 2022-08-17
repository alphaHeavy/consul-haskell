{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Misc
  ( runService
  ) where

import Import
import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T (concat)

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
                      Just x@(Ttl _) -> do
                        a <- async $ forever $ ttlFunc x
                        return $ Just a
                      _ -> return Nothing

      _foo :: () <- wait mainFunc --prevent: 'StM’ is a type function, and may not be injective
      forM_ checkAction cancel
    False -> return ()
  where
    ttlFunc (Script _ _) = undefined -- TODO: what should this be instead??
    ttlFunc (Http _) = undefined     -- TODO: what should this be instead??
    ttlFunc (Ttl x) = do
      let ttl = parseTtl x
          floorTtl = floor (fromIntegral ttl / 2 :: Double)
          delay = (ttl - floorTtl) * 1000000
      -- pause for delay, based on ttl
      liftIO $ threadDelay delay
      let checkId = T.concat["service:",fromMaybe (rsName request) (rsId request)]
      passHealthCheck client checkId
