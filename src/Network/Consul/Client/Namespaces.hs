{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
-- https://www.consul.io/api-docs/namespaces
module Network.Consul.Client.Namespaces
  ( createNamespace
  , deleteNamespace
  , getNamespace     -- readNamespace
  , listNamespaces
  , updateNamespace
  ) where

import Import

-- | TODO: Document
createNamespace :: MonadIO m => ConsulClient -> SessionRequest -> m ()
createNamespace = undefined

-- | TODO: Document
deleteNamespace :: MonadIO m => ConsulClient -> SessionRequest -> m ()
deleteNamespace = undefined

-- | TODO: Document
getNamespace :: MonadIO m => ConsulClient -> SessionRequest -> m ()
getNamespace = undefined

-- | TODO: Document
listNamespaces :: MonadIO m => ConsulClient -> SessionRequest -> m ()
listNamespaces = undefined

-- | TODO: Document
updateNamespace :: MonadIO m => ConsulClient -> SessionRequest -> m ()
updateNamespace = undefined
 
