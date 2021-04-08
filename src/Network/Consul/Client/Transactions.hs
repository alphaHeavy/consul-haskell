{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
-- https://www.consul.io/api-docs/txn
module Network.Consul.Client.Transactions
  ( createTransaction 
  ) where

import Import

-- | Create Transaction
--createTransaction :: MonadIO m => ConsulClient -> m (Maybe Transaction)
createTransaction :: MonadIO m => ConsulClient -> m ()
createTransaction = undefined
