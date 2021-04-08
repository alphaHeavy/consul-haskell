{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
-- https://www.consul.io/api-docs/query
module Network.Consul.Client.PreparedQueries
  ( createQuery
  , deleteQuery
  , executeQuery
  , explainQuery
  , getQuery
  , listQueries
  , updateQuery
  ) where

import Import

-- | TODO: Document
createQuery :: MonadIO m => ConsulClient -> SessionRequest -> m ()
createQuery = undefined

-- | TODO: Document
deleteQuery :: MonadIO m => ConsulClient -> SessionRequest -> m ()
deleteQuery = undefined

-- | TODO: Document
executeQuery :: MonadIO m => ConsulClient -> m ()
executeQuery = undefined

-- | TODO: Document
explainQuery :: MonadIO m => ConsulClient -> m ()
explainQuery = undefined

-- | TODO: Document
getQuery :: MonadIO m => ConsulClient -> m ()
getQuery = undefined

-- | TODO: Document
listQueries :: MonadIO m => ConsulClient -> m ()
listQueries = undefined

-- | TODO: Document
updateQuery :: MonadIO m => ConsulClient -> m ()
updateQuery = undefined
