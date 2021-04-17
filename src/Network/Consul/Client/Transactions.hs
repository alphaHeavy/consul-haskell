{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The functions in this module correspond to
the [Consul Transactions API](https://www.consul.io/api/txn).

This module is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

__Missing Functions__

* `createTransaction`
-}
module Network.Consul.Client.Transactions
  ( createTransaction 
  ) where

import Import

{- | Create Transaction

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
--createTransaction :: MonadIO m => ConsulClient -> m (Maybe Transaction)
createTransaction :: MonadIO m => ConsulClient -> m ()
createTransaction = undefined
